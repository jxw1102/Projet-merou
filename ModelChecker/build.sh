#!/bin/bash

rm -r doc
mkdir doc
scaladoc -d doc $(find src/ -name '*.scala')

mkdir bin
scalac -sourcepath src -d bin $(find src/ -name '*.scala')

mkdir release
jar -cf release/doc.jar doc
