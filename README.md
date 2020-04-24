# find-knockouts

Find knockouts in a VCF file.

Author: Sébastian Méric de Bellefon, Montréal Heart Institute

## Usage

```console
foo@bar:~$ stack build --test
foo@bar:~$ cp .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/find-knockouts/find-knockouts .
foo@bar:~$ ./find-knockouts genes.tab input.vcf.gz output.vcf
```

The genes.tab file contains the coordinates of known loss-of-function variants and the name of the gene they are part of. For instance:

var\_chr1\_69159	OR4F5
var\_chr1\_69336	OR4F5
var\_chr1\_69469	OR4F5
var\_chr1\_69492	OR4F5
var\_chr1\_69702	OR4F5
var\_chr1\_69745	OR4F5
var\_chr1\_69849	OR4F5
var\_chr1\_69854	OR4F5
var\_chr1\_69869	OR4F5
var\_chr1\_930221	SAMD11
var\_chr1\_930223	SAMD11
var\_chr1\_930337	SAMD11
var\_chr1\_931073	SAMD11
 

