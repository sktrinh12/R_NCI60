#!/usr/bin/python3.6
import openbabel as ob
import pybel

smarts = pybel.Smarts("c:c")

def findarom(smiles,smarts):
    ar_ring = 0
    smi = pybel.readstring("smi",smiles)
    smart_length = len(smarts.findall(smi))
    if smart_length % 6 == 0:
        ar_ring = int(smart_length / 6)
    else:
        ar_ring = 0
    return ar_ring
