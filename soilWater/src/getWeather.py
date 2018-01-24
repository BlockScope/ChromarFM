#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

t2m = "167.128"
prec = "228.128"
rad = "176.128"
d2m = "168.128"
mt2m = "202.128"

def defReq():
  return ({
    'dataset'    : "interim",
    'class'      : "ei",
    'type'       : "an",
    "stream"     : "OPER",
    "expver"     : "0001",
    "repres"     : "sh",
    "levtype"    : "sfc",
    "param"      : "",
    "time"       : "",
    "step"       : "",
    "domain"     : "g",
    "resol"      : "auto",
    "area"       : "73.5/-27/33/45",
    "grid"       : "1.5/1.5",
    "padding"    : "0",
    "expect"     : "ANY",
    "date"       : "20160201/to/20160229",
    "format"     : "netcdf",
    "target"     : ""
  })

def mkPrecReq():
    dreq = defReq()

    dreq["param"] = prec
    dreq["type"] = "fc"
    dreq["time"] = "0000/1200"
    dreq["step"] = "12"

    return dreq

def mkTempReq():
    dreq = defReq()

    dreq["param"] = t2m
    dreq["time"] = "0000/0600/1200/1800"
    dreq["step"] = "0"

    return dreq

def mkRadReq():
    dreq = defReq()

    dreq["param"] = rad
    dreq["type"] = "fc"
    dreq["time"] = "0000/1200"
    dreq["step"] = "12"

    return dreq

def mkDTempReq():
    dreq = defReq()

    dreq["param"] = d2m
    dreq["time"] = "1200/1800"
    dreq["step"] = "0"

    return dreq

def mkMinTempReq():
    dreq = defReq()

    dreq["param"] = mt2m
    dreq["time"] = "0000/1200"
    dreq["step"] = "3/6/9/12"

    return dreq

def go():
    server = ECMWFDataServer()
    
    tempReq = mkTempReq()
    tempReq["date"] = "20100101/to/20111231"
    tempReq["target"] = "../data/t2m_20102011.nc"

    dtempReq = mkDTempReq()
    dtempReq["date"] = "20100101/to/20111231"
    dtempReq["target"] = "../data/d2m_20102011.nc"

    radReq = mkRadReq()
    radReq["date"] = "20100101/to/20111231"
    radReq["target"] = "../data/ssr_20102011.nc"

    precReq = mkPrecReq()
    precReq["date"] = "20100101/to/20111231"
    precReq["target"] = "../data/tp_20102011.nc"

    mtempReq = mkMinTempReq()
    mtempReq["date"] = "20100101/to/20111231"
    mtempReq["target"] = "../data/mt2m_20102011.nc"

    for req in [tempReq, dtempReq, radReq, precReq, mtempReq]:
        server.retrieve(req)

    return
