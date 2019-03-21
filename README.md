# Basic-VEP-Parser

## Introduction

Somatic variant manual review usually starts with some basic filtration to weed out obviously incorrect calls made by variant caller(s), low read depth, and so on.  This process is usually done with ad-hoc bash scripts, using awk and/or sed, but leads to great variation.  This script, implemented in [Haskell](https://www.haskell.org/), provides a standardized method for basic filtration on somatic variant data using a filtration string.

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
