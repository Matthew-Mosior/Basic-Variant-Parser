# Basic-VEP-Parser: An ensembl-vep parser

## Introduction

Ensembl's [Variant Effect Predictor](https://github.com/Ensembl/ensembl-vep)(**vep**) provides functional annotations of genomic variants.  However, the output of **vep** can be hard to parse, especially due to the **Extra** column.  This **Extra** column contains very useful data that is hidden within semi-colon demimited fields that create problems for further analysis. This script, implemented in [Haskell](https://www.haskell.org/), provides a conversion of the standard output of **vep** to a fully tab-delimited version of **vep** output that is fully and easily accessible to downstream filtration and further parsing. 

## Prerequisites

**bvp.hs** assumes you have a the [GHC](https://www.haskell.org/ghc/) compiler and packages installed that it imports.  The easiest way to do this is to download the [Haskell Platform](https://www.haskell.org/platform/).<br/><br/>

## Installing required packages

To install the peripheral packages **bvp.hs** requires, you can call the following command assuming you have [cabal](https://www.haskell.org/cabal/), a package manager and build system for Haskell, installed on your system (it comes with the [Haskell Platform](https://www.haskell.org/platform/)).<br/><br/>
`$ cabal install [packagename]`<br/><br/>

**Required packages**
 - Text.PrettyPrint.Boxes
 - System.Process
 - Data.List.Split 
 - System.Temporary
