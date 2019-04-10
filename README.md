# Basic-Variant-Parser: A Variant File Parser

## Introduction

[Ensembl's](https://github.com/Ensembl) [Variant Effect Predictor](https://github.com/Ensembl/ensembl-vep) (**vep**) provides functional annotations of genomic variants.  However, the output of **vep** can be hard to parse, especially due to the **Extra** column.  This **Extra** column contains very useful data that is nested within semi-colon demimited fields that creates problems for further analysis. This script, implemented in [Haskell](https://www.haskell.org/), provides a conversion of the standard output of **vep** to a fully tab-delimited version of **vep** output that is fully and easily accessible to downstream filtration and further parsing. 

## Prerequisites

**bvp.hs** assumes you have a the [GHC](https://www.haskell.org/ghc/) compiler and packages installed that it imports.  The easiest way to do this is to download the [Haskell Platform](https://www.haskell.org/platform/).<br/><br/>

## Installing required packages

To install the peripheral packages **bvp.hs** requires, you can call the following command assuming you have [cabal](https://www.haskell.org/cabal/), a package manager and build system for Haskell, installed on your system (it comes with the [Haskell Platform](https://www.haskell.org/platform/)).<br/><br/>
`$ cabal install [packagename]`<br/><br/>

**Required packages**
- Data.List 
- Data.List.Split 
- Data.Ord 
- Data.Tuple 
- System.Console.GetOpt 
- System.Directory 
- System.Environment 
- System.Exit 
- System.IO 
- System.IO.Temp 
- System.Process 
 
 ## Input

A prerequisite for getting useful output from this script is to have the correct input file structure.  This script requires that you provide a file that was produced using **vep**, or a **variant-calling format (vcf)** file.

## Usage

**bvp.hs** is easy to use.<br/><br/>
You can call it using the **runghc** command provided by the GHC compiler as such:<br/>
`$ runghc bvp.hs example.vep`<br/><br/>
For maximum performance, please compile and run the source code as follows:<br/>
`$ ghc -O2 -o BVP bvp.hs`<br/>
`$ ./BVP example.vep`<br/>

## Arguments

**bvp.hs** has few different command line arguments:<br/>
```
Basic Variant Parser, Copyright (c) 2019 Matthew Mosior.
Usage: bvp [-vV?IoOgG] [file]
  -v          --verbose             Output on stderr.
  -V, -?      --version             Show version number.
  -I IN       --InFormat=IN         The format of the input file.
  -O OUT      --OutFormat=OUT       The format of the output file.
  -o OUTFILE  --OutputFile=OUTFILE  The output file.
  -g          --GzipIn              Gzipped input file?
  -G          --GzipOut             Gzipped output file?
              --help                Print this help message.
```
The `-v` option, the `verbose` option, will provide a full error message.<br/>
The `-V` option, the `version` option, will show the version of `bvp` in use.<br/>
The `-I` option, the `InFormat` option, specifies the format of the input file, and is required to run `bvp.hs`.<br/>
The `-O` option, the `OutFormat` option, specifies the format of the output file, and is required to run `bvp.hs`.<br/>
The `-o` option, the `outputfile` option, is used to output the operation on the input file into a output file, whose name is specified by the user.<br/>
The `-g` option, the `GzipIn` option, specifies that the input file is gzipped.<br/>
The `-G` option, the `GzipOut` option, specifies that the output file is to be gzipped.<br/>
Finally, the `--help` option outputs the `help` message seen above.

## Docker 

A docker-based solution (Dockerfile) is availible in the corresponding [repository](https://github.com/Matthew-Mosior/Basic-VEP-Parser---Docker).  Currently, this Dockerfile assumes that you run docker interactively.

## Credits

Documentation was added March 2019.<br/>
Author : [Matthew Mosior](https://github.com/Matthew-Mosior)
