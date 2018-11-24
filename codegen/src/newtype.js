// @flow

import { invariant } from './util'



const isBoxed = (name: string) => name[0].toUpperCase() === name[0] && name[0] !== '#'
const isBare = (name: string) => name[0].toLowerCase() === name[0]

export opaque type TLNameLC: string = string

export const createNameLC = (name: string): TLNameLC => {
  invariant(isBare(name) && !name.includes('.'), `'${name}' is not a bare type identifier.`)
  return name
}

export opaque type TLNameUC: string = string

export const createNameUC = (name: string): TLNameUC => {
  invariant(isBoxed(name) && !name.includes('.'), `'${name}' is not a boxed type identifier.`)
  return name
}

export opaque type Nat: number = number

export const createNat = (n: number): Nat => {
  invariant(n >= 0 && Math.floor(n) === n, `${n} is not a natural number.`)
  return n
}
