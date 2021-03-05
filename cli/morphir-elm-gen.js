#!/usr/bin/env node
'use strict'

// NPM imports
const path = require('path')
const commander = require('commander')
const cli = require('./cli')

// Set up Commander
const program = new commander.Command()
program
    .name('morphir-elm gen')
    .description('Generate code from Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from.', 'morphir-ir.json')
    .option('-o, --output <path>', 'Target location where the generated code will be saved.', './dist')
    .option('-t, --target <type>', 'Language to Generate (Scala | SpringBoot | cypher | triples).', 'Scala')
    .option('-tv, --target-version <version>', 'Language version to Generate.', '2.11')
    .option('-c, --copy-deps', 'Copy the dependencies used by the generated code to the output path.', false)
    .parse(process.argv)


cli.gen(program.input, path.resolve(program.output), program.opts())
    .then(() => {
        console.log("Done.")
    })
    .catch((err) => {
        console.error(err)
        process.exit(1)
    })

