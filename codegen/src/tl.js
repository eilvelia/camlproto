// @flow

/*:: import * as Ast from 'tl-parser/ast.h' */

import { unnest } from 'ramda'

import { invariant, capitalize } from './util'

import {
  type TLNameLC,
  createNameLC,
  type TLNameUC,
  createNameUC,
  type Nat,
  createNat
} from './newtype'

export class TLError extends Error {}

type Loc = { offset: number, line: number, column: number }
class ErrorOnPos extends Error {
  constructor(node: { start: Loc }, str: string) {
    super(str + ` [${node.start.line}:${node.start.column}]`)
  }
}

export class TLErrorOnPos extends ErrorOnPos {}
export class NotSupported extends ErrorOnPos {}

const builtins =
  ['int', 'nat', 'long', 'string', 'double', 'int128', 'int256', 'bytes']

const boxedBuiltins = builtins.map(capitalize)

export const isBuiltin = (name: string) => builtins.includes(name)
export const isBuiltinType = (name: string) => boxedBuiltins.includes(name)

// type TLBuiltin = {
//   type: 'Builtin',
//   value:
//     'int' | 'nat' | 'long' | 'string' | 'double' | 'int128' | 'int256' | 'bytes'
// }

// type BuiltinLen =
//   | { type: 'LenStatic', value: number }
//   | { type: 'LenDynamic' }
//
// const LenStatic = (value: number) => ({ type: 'LenStatic', value })
// const lenDynamic = { type: 'LenDynamic' }
//
// const lenOfBuiltin = (b: TLBuiltin): BuiltinLen => {
//   switch (b.value) {
//     case 'int': return LenStatic(4)
//     case 'nat': return LenStatic(4)
//     case 'long': return LenStatic(8)
//     case 'string': return lenDynamic
//     case 'double': return LenStatic(8)
//     case 'int128': return LenStatic(16)
//     case 'int256': return LenStatic(32)
//     case 'bytes': return lenDynamic
//     default: return (b.value: empty)
//   }
// }

export type TLTypeId =
  | { type: 'BoxedTypeId', ns: string | null, name: TLNameUC }
  | { type: 'BareTypeId', ns: string | null, name: TLNameLC }

export const createBoxedTypeId = (str: string): TLTypeId => {
  const [ns, name] = str.includes('.') ? str.split('.') : [null, str]
  return { type: 'BoxedTypeId', ns, name: createNameUC(name) }
}

export const createBareTypeId = (str: string): TLTypeId => {
  const [ns, name] = str.includes('.') ? str.split('.') : [null, str]
  return { type: 'BareTypeId', ns, name: createNameLC(name) }
}

export const eqTypeId = (t1: TLTypeId, t2: TLTypeId) => {
  return t1.type === t2.type
    && t1.ns === t2.ns
    && t1.name === t2.name
}

export const convertTypeIdent = (node: Ast.TypeIdentifier): TLTypeId => {
  switch (node.type) {
    case 'BoxedTypeIdentifier': return createBoxedTypeId(node.name)
    case 'SimpleTypeIdentifier': return createBareTypeId(node.name)
    case 'HashTypeIdentifier': return createBareTypeId('nat')
    default: return (node.type: empty)
  }
}

const isValidVarIdent = (name: string) =>
  /^[a-z][a-zA-Z0-9_]*$/.test(name)

export const varFromIdent = (node: Ast.TypeIdentifier): TLNameLC | null => {
  switch (node.type) {
    case 'SimpleTypeIdentifier': return isValidVarIdent(node.name)
      ? createNameLC(node.name)
      : null
    default: return null
    // default: throw new TLErrorOnPos(node, `${node.name} is not valid variable identifier`)
  }
}

export type TLExpr =
  | { type: 'NatExpr', expr: TLNatExpr }
  | { type: 'TypeExpr', expr: TLTypeExpr }

export const createNatExpr = (expr: TLNatExpr): TLExpr => ({ type: 'NatExpr', expr })
export const createTypeExpr = (expr: TLTypeExpr): TLExpr => ({ type: 'TypeExpr', expr })

export type TLNatExpr =
  | { type: 'NatExprValue', value: Nat }
  | { type: 'NatExprVar', id: TLNameLC }

export const createNatValue = (n: number) => ({ type: 'NatExprValue', value: createNat(n) })
export const createNatVar = (id: TLNameLC) => ({ type: 'NatExprVar', id })

export type TLTypeExpr =
  | { type: 'TypeExprId', id: TLTypeId }
  | { type: 'TypeExprList', list: Array<TLTypeExpr> }
  | { type: 'TypeExprBare', expr: TLTypeExpr }
  | { type: 'TypeExprBang', expr: TLTypeExpr }

export const createExprTypeId = (id: TLTypeId) => ({ type: 'TypeExprId', id })
export const createExprList = (list: TLTypeExpr[]) => ({ type: 'TypeExprList', list })
export const createExprBare = (expr: TLTypeExpr) => ({ type: 'TypeExprBare', expr })
export const createExprBang = (expr: TLTypeExpr) => ({ type: 'TypeExprBang', expr })

export const convertOperator = (op: Ast.EOperator): TLTypeExpr => {
  switch (op.kind) {
    case '%': return createExprBare(convertTypeExpression(op.expression))
    case '!': return createExprBang(convertTypeExpression(op.expression))
    case '+': throw new NotSupported(op, 'Operator + is not supported')
    default: return (op.kind: empty)
  }
}

export const convertExpression = (expr: Ast.Expression): TLExpr => {
  const natExpr = convertNatExpression(expr)
  if (natExpr)
    return createNatExpr(natExpr)
  const typeExpr = convertTypeExpression(expr)
  return createTypeExpr(typeExpr)
}

export const convertNatExpression = (expr: Ast.Expression): TLNatExpr | null => {
  switch (expr.type) {
    // TODO: rename to `EIdentifier` in parser
    case 'ETypeIdentifier':
      const variable = varFromIdent(expr.id)
      return variable ? createNatVar(variable) : null
    case 'ENat': return createNatValue(expr.value)
    default: return null
  }
}

export const convertTypeExpression = (expr: Ast.Expression): TLTypeExpr => {
  switch (expr.type) {
    case 'ETypeIdentifier': return createExprTypeId(convertTypeIdent(expr.id))
    case 'ENat': throw new TLErrorOnPos(expr, 'Nat is not a type expression')
    case 'EExpression': return createExprList(expr.subexpressions.map(convertTypeExpression))
    case 'EOperator': return convertOperator(expr)
    case 'EMultiArg': throw new NotSupported(expr, 'Repetitions are not supported')
    default: return (expr.type: empty)
  }
}

export type TLConditional = {
  type: 'Conditional',
  id: TLNameLC,
  bit: Nat /* | null */
}

export type TLArg = {
  type: 'Arg',
  id: TLNameLC /* | null */,
  cond: TLConditional | null,
  argType: TLTypeExpr
}

export type TLOptArg = {
  type: 'OptArg',
  id: string,
  argType: 'Type' | 'nat'
}

export const convertCond = (cond: Ast.ConditionalDefinition): TLConditional => {
  if (cond.nat == null)
    throw new NotSupported(cond, 'Conditionals without number are not supported')
  if (cond.nat > 32)
    throw new TLErrorOnPos(cond, 'Cant\'t use more than 32 bits for conditional')
  return {
    type: 'Conditional',
    bit: createNat(cond.nat),
    id: createNameLC(cond.id.name)
  }
}

export const convertArg = (arg: Ast.Argument): TLArg => {
  if (arg.id.type === 'EmptyVariableIdentifier')
    throw new NotSupported(arg, 'Unlabeled arguments are not supported')
  const name = createNameLC(arg.id.name)
  return {
    type: 'Arg',
    id: name,
    cond: arg.conditionalDef && convertCond(arg.conditionalDef),
    argType: convertTypeExpression(arg.argType.expression)
  }
}

export const convertOptArg = (arg: Ast.OptionalArgument): TLOptArg => {
  const argType = convertTypeExpression(arg.argType.expression)
  if (argType.type !== 'TypeExprId')
    throw new TLError(`${arg.id.name}: Nested type exprs in opt args are not allowed`)
  const { name } = argType.id
  if (name !== 'Type' && name !== 'nat')
    throw new TLError(`Optional arg ${arg.id.name} should be of type Type or #`)
  return { type: 'OptArg', id: arg.id.name, argType: name }
}

export type TLCombId = {
  type: 'CombId',
  ns: string | null,
  name: TLNameLC
}

export const createCombId = (str: string): TLCombId => {
  const [ns, name_] = str.includes('.') ? str.split('.') : [null, str]
  const name = createNameLC(name_)
  return { type: 'CombId', ns, name }
}

export const eqCombId = (t1: TLCombId, t2: TLCombId) => {
  return t1.ns === t2.ns && t1.name === t2.name
}

export const eqCombIdAndTypeId = (t1: TLCombId, t2: TLTypeId) => {
  return t1.ns === t2.ns && t1.name === t2.name
}

export type TLCombinator = {
  type: 'Combinator',
  id: TLCombId,
  magic: string,
  optArgs: TLOptArg[],
  args: TLArg[],
  resultType: TLTypeExpr
}

const normalizeResultType = (r: Ast.ResultType): Ast.Expression => {
  // XXX
  const idexpr = {
    type: 'ETypeIdentifier',
    start: r.start,
    end: r.end,
    id: r.id
  }
  const exprList = r.expression.subexpressions
  if (exprList.length === 0)
    return idexpr
  return {
    type: 'EExpression',
    start: r.start,
    end: r.end,
    subexpressions: [idexpr].concat(exprList)
  }
}

const convertCombId = (node: Ast.FullCombinatorIdentifier): [TLCombId, string] => {
  switch (node.type) {
    case 'FullCombinatorName': return [createCombId(node.name), node.magic]
    case 'ShortCombinatorName':
      throw new NotSupported(node, 'Combinators without magic are not supported')
    case 'EmptyCombinatorName':
      throw new NotSupported(node, 'Empty combinator names are not supported')
    default: return (node.type: empty)
  }
}

export const convertComb = (comb: Ast.CombinatorDeclaration): TLCombinator => {
  const optArgs = comb.optionalArgs.map(convertOptArg)
  const args = comb.args.map(convertArg)
  const resultType = convertTypeExpression(normalizeResultType(comb.resultType))
  const [id, magic] = convertCombId(comb.id)
  return { type: 'Combinator', id, magic, optArgs, args, resultType }
}

export const getTypeConstructor = (t: TLTypeExpr): TLTypeId => {
  switch (t.type) {
    case 'TypeExprId': return t.id
    case 'TypeExprList': return getTypeConstructor(t.list[0])
    case 'TypeExprBare': return getTypeConstructor(t.expr)
    case 'TypeExprBang': return getTypeConstructor(t.expr)
    default: return (t.type: empty)
  }
}

export const getAllTypeIds = (t: TLTypeExpr): TLTypeId[] => {
  switch (t.type) {
    case 'TypeExprId': return [t.id]
    case 'TypeExprList': return unnest(t.list.map(getAllTypeIds))
    case 'TypeExprBare': return getAllTypeIds(t.expr)
    case 'TypeExprBang': return getAllTypeIds(t.expr)
    default: return (t.type: empty)
  }
}

export type TLType = {
  type: 'TLType',
  id: TLTypeId
}

export const createTLType = (id: TLTypeId): TLType => ({ type: 'TLType', id })

export type TLObject = TLCombinator | TLType
