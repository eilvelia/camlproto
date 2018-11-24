// @flow

class InvariantViolation extends Error {}
export type InvariantViolationT = InvariantViolation
export function invariant (cond: boolean, msg: string) {
  if (!cond) throw new InvariantViolation(msg)
}

export const capitalize = (str: string) => str[0].toUpperCase() + str.slice(1)
export const uncapitalize = (str: string) => str[0].toLowerCase() + str.slice(1)
