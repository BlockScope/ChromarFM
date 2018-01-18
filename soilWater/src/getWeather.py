#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

def defReq():
  {
    'dataset'    : "interim",
    'class'      : "ei",
    'type'       : "an",
    "stream"     : "OPER",
    "expver"     : "0001",
    "repres"     : "sh",
    "levtype"    : "sfc",
    "param"      : "167.128",
    "time"       : "0000/0600/1200/1800",
    "step"       : "0",
    "domain"     : "g",
    "resol"      : "auto",
    "area"       : "73.5/-27/33/45",
    "grid"       : "1.5/1.5",
    "padding"    : "0",
    "expect"     : "ANY",
    "date"       : "20160201/to/20160229",
    "format"     : "netcdf",
    "target"     : "../data/t2m_201602.nc"
}

def time(req, t):
    req["time"] = t
    return req

def param(req, p):
    req["param"] = p
    return req

def target(req, targ):
    req["target"] = targ
    return req

def getYear():
    pass

def go():
    server = ECMWFDataServer()
    server.retrieve({
        'dataset'    : "interim",
        'class'      : "ei",
        'type'       : "an",
        "stream"     : "OPER",
        "expver"     : "0001",
        "repres"     : "sh",
        "levtype"    : "sfc",
        "param"      : "167.128",
        "time"       : "0000/0600/1200/1800",
        "step"       : "0",
        "domain"     : "g",
        "resol"      : "auto",
        "area"       : "73.5/-27/33/45",
        "grid"       : "1.5/1.5",
        "padding"    : "0",
        "expect"     : "ANY",
        "date"       : "20160101/to/20160229",
        "format"     : "netcdf",
        "target"     : "../data/t2m_201602.nc"
    })
