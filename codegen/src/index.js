// @flow

import fs from 'fs'

import { run } from './caml'

const input = process.argv[2]
const src = fs.readFileSync(input).toString()

const output = run(src)

console.log(output)
