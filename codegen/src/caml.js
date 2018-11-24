// @flow

import { parse } from 'tl-parser'

/*:: import * as Ast from 'tl-parser/ast.h' */

import {
  isBuiltin,
  isBuiltinType,
  eqCombIdAndTypeId,
  getTypeConstructor,
  getAllTypeIds,
  createTLType,
  eqTypeId,
  convertComb
} from './tl'

import type {
  TLObject,
  TLTypeExpr,
  TLCombId,
  TLTypeId,
  TLArg,
  TLOptArg,
  TLCombinator,
  TLType
} from './tl'

import { uniq } from 'ramda'

import { invariant, capitalize } from './util'

// import {
//   type TLNameLC,
//   type TLNameUC,
//   type Nat,
// } from './newtype'

const combBlacklist = new Set(
  ['null', 'vector', 'boolTrue', 'boolFalse'])
const typeBlacklist = new Set(
  ['Null', 'Vector', 'Bool'])

const isSkippedComb = (name: string) => combBlacklist.has(name) || isBuiltin(name)
const isSkippedType = (name: string) => typeBlacklist.has(name)

const isSkippedTLObject = (obj: TLObject): boolean => {
  // return isSkippedComb(obj.id.name) || isSkippedType(obj.id.name)
  switch (obj.type) {
    case 'Combinator': return isSkippedComb(obj.id.name)
    case 'TLType': return isSkippedType(obj.id.name)
    default: return (obj.type: empty)
  }
}

const filterDeclarations = (ds: Ast.Declaration[]): Ast.CombinatorDeclaration[] => ds
  .map(d => d.type === 'CombinatorDeclaration' ? d : null)
  .filter(Boolean) // flow
  .filter(c => !isSkippedComb(c.id.name))

// const CamlAny = '(Caml.Obj.magic ())'

const combNameToCaml = (name: /*TLNameLC*/ string): string => {
  if (isBuiltin(name))
    return `TL${capitalize(name)}`

  return `C_${name}`
}

const typeNameToCaml = (name: /*TLNameUC*/ string): string => {
  if (isBuiltinType(name))
    return `TLT${capitalize(name)}`

  switch (name) {
    case 'Bool': return 'TLBool'
  }

  return capitalize(name)
  // return name
}

const argNameToCaml = (name: string): string => {
  switch (name) {
    case 'type': return 'type_'
    case 'private': return 'private_'
    default: return name
  }
}

const showTLTypeId = (t: TLTypeId): string => {
  return t.ns ? `${t.ns}_${t.name}` : t.name
}

const showCombId卐 = (t: TLCombId): string => {
  return t.ns ? `${t.ns}_${t.name}` : t.name
}

const addIndent = (str: string) => str
  .replace(/\n/g, '\n\t')
  .replace(/^/, '\t')

const addIndentN = n => str =>
  n < 1 ? str : addIndentN(n - 1)(addIndent(str)) // yeah, no tco, blah blah. meh.

const buildTypeId = (id: TLTypeId): string => {
  switch (id.type) {
    case 'BareTypeId': return combNameToCaml(showTLTypeId(id))
    case 'BoxedTypeId': return typeNameToCaml(showTLTypeId(id))
    default: return (id.type: empty)
  }
}

const showTypeExpr = (t: TLTypeExpr): string => {
  switch (t.type) {
    case 'TypeExprId': return buildTypeId(t.id)
    case 'TypeExprList': return t.list.map(showTypeExpr).join('')
    case 'TypeExprBare': throw new Error('Bare modifiers are not supported')
    case 'TypeExprBang': return showTypeExpr(t.expr)
    default: return (t.type: empty)
  }
}

const typeExprToFAppl = (t: TLTypeExpr): string => {
  switch (t.type) {
    case 'TypeExprId': return buildTypeId(t.id)
    case 'TypeExprList':
      invariant(t.list.length > 0, 'TypeExprList with length 0')
      const [typeConstr, ...typeParams] = t.list
      const typeParamsStr = typeParams
        .map(expr => `(${typeExprToFAppl(expr)})`)
        .join('')
      return typeExprToFAppl(typeConstr) + typeParamsStr
    case 'TypeExprBare': throw new Error('Bare modifiers are not supported [2]')
    case 'TypeExprBang': return typeExprToFAppl(t.expr)
    default: return (t.type: empty)
  }
}

const FLAGS = 'flags'
const CAML_TLOBJECT = 'TLObject'
const CAML_TLFUNC = 'TLFunc'
const CAML_NO_ARGS_CONSTR = 'C' // Name of value constructor for combinators without arguments

type CombKind = 'Func' | 'Constr'

const isFunctionalOptArg = (optArgId: TLTypeId | string /* XXX */, args: TLArg[]) =>
  args.some(({ argType }) => {
    return argType.type === 'TypeExprBang'
      && argType.expr.type === 'TypeExprId'
      && argType.expr.id.name === (typeof optArgId === 'string' ? optArgId : optArgId.name)
  })

const isFnOptArgInResult = (result: TLTypeExpr, args: TLArg[]): boolean => {
  switch (result.type) {
    case 'TypeExprId': return isFunctionalOptArg(result.id, args)
    case 'TypeExprList': return result.list.some(e => isFnOptArgInResult(e, args))
    case 'TypeExprBare': return isFnOptArgInResult(result.expr, args)
    case 'TypeExprBang': return isFnOptArgInResult(result.expr, args)
    default: return (result.type: empty)
  }
}

const buildComb = (comb: TLCombinator, k: CombKind): string => {
  const moduleName = combNameToCaml(showCombId卐(comb.id))

  const resultTypeStr = typeNameToCaml(showTypeExpr(comb.resultType))

  const { magic } = comb

  const validOptArgs = comb.optArgs.filter(a => a.argType === 'Type')
  const typeParameters = validOptArgs.map(a => a.id)
  const functorArguments_ = typeParameters
    .map(name => isFunctionalOptArg(name, comb.args)
      ? `(${name}: ${CAML_TLFUNC})`
      : `(${name}: ${CAML_TLOBJECT})`)
    .join(' ')
  const functorArguments = functorArguments_ ? ` ${functorArguments_}` : ''

  const fnOptArgInResult = isFnOptArgInResult(comb.resultType, comb.args)
  const resultIsFnOptArg = fnOptArgInResult && comb.resultType.type === 'TypeExprId'

  let flagsExists: boolean = false
  const flagsType = combNameToCaml('nat')

  const args = comb.args.map(arg => {
    if (arg.id === FLAGS && showTypeExpr(arg.argType) == flagsType) {
      flagsExists = true
      return
    }
    const condBit = arg.cond ? arg.cond.bit : null
    const typeConstrName = showTLTypeId(getTypeConstructor(arg.argType))
    const typeIsOptArgRef = typeParameters.some(name => name === typeConstrName)
    return {
      id: argNameToCaml(arg.id),
      condBit,
      argType: arg.argType,
      argTypeStr: typeNameToCaml(showTypeExpr(arg.argType)),
      typeIsOptArgRef
    }
  }).filter(Boolean)

  // const moduleDeclarations: Set<string> = new Set()
  // const moduleSignatures: Set<string> = new Set()
  //
  // args
  //   .filter(e => e.argType.type === 'TypeExprList')
  //   .forEach(e => {
  //     const moduleName = e.argTypeStr
  //     const value = typeExprToFAppl(e.argType)
  //     moduleDeclarations.add(
  //       `\tmodule ${moduleName} = ${value}`)
  //     moduleSignatures.add(
  //       `\tmodule ${moduleName}: sig include module type of ${value} end`)
  //   })
  //
  // if (comb.resultType.type === 'TypeExprList') {
  //   const value = typeExprToFAppl(comb.resultType)
  //   moduleDeclarations.add(
  //     `\tmodule ${resultTypeStr} = ${value}`)
  //   moduleSignatures.add(
  //     `\tmodule ${moduleName}: sig include module type of ${value} end`)
  // }

  const moduleDeclarations_ = args
    .filter(e => e.argType.type === 'TypeExprList')
    .map(e => {
      const moduleName = e.argTypeStr
      const value = typeExprToFAppl(e.argType)
      return  `\tmodule ${moduleName} = ${value}`
    })

  if (comb.resultType.type === 'TypeExprList') {
    moduleDeclarations_.push(
      `\tmodule ${resultTypeStr} = ${typeExprToFAppl(comb.resultType)}`)
  }

  const moduleDeclarations = uniq(moduleDeclarations_)

  const fields = args.map(e => {
    const option = e.condBit != null ? ' option' : ''
    return `\t\t${e.id}: ${e.argTypeStr}.t${option};`
  })

  let encodeStart = ''

  if (flagsExists) {
    const arr = []
    arr.push('let flags = ref 0l in')
    arr.push('begin [@warning "-33"] Int32.(')
    for (const e of args) {
      if (e.condBit != null) {
        const { condBit } = e
        arr.push(`\tmatch t.${e.id} with`)
        arr.push(`\t\tSome _ -> flags := !flags lor (1l lsl ${condBit}) | None -> ();`)
      }
    }
    arr.push(') end;')
    arr.push(`${flagsType}.encode enc !flags;`)
    encodeStart += arr.map(addIndentN(3)).join('\n')
  }

  const encodes = args.map(e => {
    const encodeFn = (e.typeIsOptArgRef && fnOptArgInResult)
      ? 'encode_boxed'
      : 'encode'
    if (e.condBit != null) {
      return `\t\t\tmatch t.${e.id} with Some x -> ${e.argTypeStr}.${encodeFn} enc x | None -> ();`
    }
    return `\t\t\t${e.argTypeStr}.${encodeFn} enc t.${e.id};`
  })

  const decodeStart = flagsExists
    ? `\t\t\tlet [@warning "-26"] flags = ${flagsType}.decode dec in`
    : ''

  const decodes = args.map(e => {
    if (e.condBit != null) {
      return [
        `let ${e.id} =`,
        `\tif Int32.(flags land (1l lsl ${e.condBit}) <> 0l) then`,
        `\t\tSome (${e.argTypeStr}.decode dec)`,
        `\telse None`,
        `in`
      ].map(addIndentN(3)).join('\n')
    }
    return `\t\t\tlet ${e.id} = ${e.argTypeStr}.decode dec in`
  })

  const decodeEnd = args.length > 0
    ? `\t\t\t{ ${args.map(e => e.id).join('; ')} }`
    : `\t\t\t${CAML_NO_ARGS_CONSTR}`

  const nameOfMakeFunctor = k === 'Constr' ? 'MakeConstrR' : 'MakeFuncR'
  const nameOfCombModuleSig = k === 'Constr' ? 'TLConstr' : 'TLFunc'

  // const camlResultType = (k === 'Func' && resultIsFnOptArg)
  //   ? `type result_type = ${resultTypeStr}.result_type`
  //   : `type result_type = ${resultTypeStr}.t`

  const camlResultM = (k === 'Func' && resultIsFnOptArg)
    ? `module ResultM = ${resultTypeStr}.ResultM`
    : `module ResultM = ${resultTypeStr}`

  const typeT = args.length > 0
    ? `\ttype t = {\n${fields.join('\n')}\n\t}`
    : `\ttype t = ${CAML_NO_ARGS_CONSTR}`

  const str = [
    // `${k === 'Constr' ? 'and' : 'module'} ${moduleName}${functorArguments}: sig`,
    // `${[...moduleSignatures].join('\n')}`,
    // `\t${typeT}`,
    // k === 'Func' ? `\t${camlResultType}` : '',
    // `\tinclude ${nameOfCombModuleSig} with type t := t`,
    // k === 'Func' ? '\t\tand type result_type := result_type' : '',
    // `end = struct`,
    // `${[...moduleDeclarations].join('\n')}`,
    `module ${moduleName}${functorArguments} = struct`,
    moduleDeclarations.join('\n'),
    typeT,
    `\tinclude ${nameOfMakeFunctor}(struct`,
    `\t\ttype nonrec t = t`,
    k === 'Func' ? `\t\t${camlResultM}` : '',
    `\t\tlet magic = 0x${magic}l`,
    args.length > 0 ? `\t\tlet encode enc t =` : `\t\tlet encode _ _ =`,
    encodeStart,
    encodes.join('\n'),
    args.length > 0 ? '' : '\t\t\t()',
    `\t\t;;`,
    args.length > 0 ? `\t\tlet decode dec =` : `\t\tlet decode _ =`,
    decodeStart,
    decodes.join('\n'),
    decodeEnd,
    `\tend)`,
    `end`,
  ].filter(str => str.length > 0).join('\n')

  return str
}

const buildConstr = (comb: TLCombinator) => buildComb(comb, 'Constr')
const buildFunc = (comb: TLCombinator) => buildComb(comb, 'Func')

// Currently can't handle definitions of polymorphic boxed types.
// But for now in official schemas only Vector is polymorphic type,
// and it is parsed manually, so it's not required.
const buildType = (type: TLType, constrs: TLCombinator[]): string => {
  const moduleName = typeNameToCaml(showTLTypeId(type.id))

  invariant(constrs.length > 0, `${moduleName}: 0 constructors`)

  const camlConstrNames = constrs
    .map(comb => combNameToCaml(showCombId卐(comb.id)))

  const valueConstructors = camlConstrNames.map(name => {
    return `\t\t| ${name} of ${name}.t`
  })

  const encodes = camlConstrNames.map(name => {
    return `\t\t\t| ${name} x -> ${name}.encode_boxed enc x`
  })

  const decodes = camlConstrNames.map(name => {
    return `\t\t\t| x when x = ${name}.magic -> ${name} (${name}.decode dec)`
  })

  const str = [
    // `and ${moduleName}: sig`,
    // `\t type t =`,
    // `${valueConstructors.join('\n')}`,
    // `\tinclude TLType with type t := t`,
    // `end = struct`,
    `module ${moduleName} = struct`,
    `\ttype t =`,
    `${valueConstructors.join('\n')}`,
    `\tinclude MakeTLTypeR(struct`,
    `\t\ttype nonrec t = t`,
    `\t\tlet encode enc t = match t with`,
    `${encodes.join('\n')}`,
    `\t\tlet decode dec =`,
    `\t\t\tlet magic = Decoder.read_int32_le dec in`,
    `\t\t\tlet open Int32 in`,
    `\t\t\tmatch magic with`,
    `${decodes.join('\n')}`,
    `\t\t\t| x -> raise @@ DeserializationError ("Invalid ${moduleName} magic " ^ to_string x)`,
    `\tend)`,
    `end`
  ].filter(str => str.length > 0).join('\n')

  return str
}

const findConstrs = (t: TLType, allConstrs: TLCombinator[]): TLCombinator[] =>
  allConstrs.reduce((acc, comb) => {
    const result = getTypeConstructor(comb.resultType)
    invariant(result.type === 'BoxedTypeId', 'Bare type in result')
    if (eqTypeId(result, t.id))
      acc.push(comb)
    return acc
  }, [])

const buildTLObject = (obj: TLObject, allConstrs: TLCombinator[]): string => {
  switch (obj.type) {
    case 'Combinator': return buildConstr(obj)
    case 'TLType':
      const constrs = findConstrs(obj, allConstrs) // not so efficient but who cares
      return buildType(obj, constrs)
    default: return (obj.type: empty)
  }
}

export const ocamlCodeGen = (
  constructors: TLCombinator[],
  functions: TLCombinator[]
): string => {
  // const types = getAllTLTypes(constructors)

  const tlObjects = reorder(constructors)
    .filter(o => !isSkippedTLObject(o))

  // console.log('tlObjects', tlObjects)

  const tlObjectStr = tlObjects
    .map(o => buildTLObject(o, constructors))
    .join('\n\n')
  // const constrStr = constructors.map(buildConstr).join('\n\n')
  const funcStr = functions
    .map(buildFunc)
    .join('\n\n')

  const str = [
    '(* AUTOGENERATED *)',
    '',
    'open! Base',
    'open TL',
    'open Types',
    'open Builtin',
    // '',
    // 'module rec M_empty: sig end = struct end',
    '',
    // '(* --- Constructors --- *)',
    // '',
    // constrStr,
    // '',
    // '(* --- Types --- *)',
    // '',
    // typeStr,
    tlObjectStr,
    '',
    '(* --- Functions --- *)',
    '',
    funcStr
  ].join('\n')

  return str.replace(/\t/g, '  ')
}

import * as graphlib from '@dagrejs/graphlib'

const genRandomId = () => Math.random().toString().slice(2, 12)

// TODO: handle recursive tl types
const reorder = (constructors: TLCombinator[]): TLObject[] => {
  const graph = new graphlib.Graph()

  const cache: Map<string /* obj name */, string /* random id */> = new Map()
  const map: Map<string /* random id */, TLObject> = new Map()

  const addObj = (obj: TLObject): string /* id */ => {
    const serialized = JSON.stringify(obj.id)
    const maybeId = cache.get(serialized)
    if (maybeId) return maybeId
    const newId = genRandomId()
    cache.set(serialized, newId)
    map.set(newId, obj)
    return newId
  }

  const findConstr = (id: TLTypeId) => {
    const constr = constructors.find(comb => eqCombIdAndTypeId(comb.id, id))
    // if (!constr) console.error(`Warning: Constr ${showTLTypeId(id)} not found.`)
    return constr
  }

  // console.dir(constructors, { depth: null })

  for (const constr of constructors) {
    const randConstrId = addObj(constr)

    const resultType = createTLType(getTypeConstructor(constr.resultType))
    const randResultTypeId = addObj(resultType)
    graph.setEdge(randConstrId, randResultTypeId)

    for (const arg of constr.args) {
      for (const typeId of getAllTypeIds(arg.argType)) {
        switch (typeId.type) {
          case 'BoxedTypeId':
            const tlType = createTLType(typeId)
            const randTypeId = addObj(tlType)
            graph.setEdge(randTypeId, randConstrId)
            // graph.setEdge(randConstrId, randTypeId)
            break
          case 'BareTypeId':
            const constr = findConstr(typeId)
            if (!constr) break
            const newConstrId = addObj(constr)
            graph.setEdge(newConstrId, randConstrId)
            // graph.setEdge(randConstrId, newConstrId)
            break
          default: (typeId.type: empty)
        }
      }
    }
  }

  if (!graphlib.alg.isAcyclic(graph)) {
    const cycles = graphlib.alg.findCycles(graph)
      .map(arr => arr.map(id => map.get(id)))
      .map(arr => JSON.stringify(arr))
    throw new Error(`Cycles: ${cycles.join('\n')}`)
  }

  const sorted = graphlib.alg.topsort(graph)

  const tlObjects = sorted.map(id => {
    const tlObject = map.get(id)
    if (!tlObject) throw new Error(`Invalid id ${id}`)
    return tlObject
  })

  // console.log(tlObjects)

  return tlObjects
}

export const run = (src: string): string => {
  const ast = parse(src)

  // console.dir(ast, { depth: null })

  const constructors = filterDeclarations(ast.constructors.declarations)
  const functions = filterDeclarations(ast.functions.declarations)

  const betterConstrs = constructors.map(convertComb)
  const betterFuncs = functions.map(convertComb)

  return ocamlCodeGen(betterConstrs, betterFuncs)
}
