# Trelliscope Typescript type and class definitions and utilities

This directory provides interface and class definitions for Trelliscope objects along with utilities for generating test cases to test the object outputs from R (and eventually Python).

## Setup

```
yarn install
```

## Generate json files of instantiated objects

```
yarn cases
```

## Generate Python classes from Typescript interfaces

Experimental...

```shell
# pip install ts2python
ts2python meta_types.ts
```
