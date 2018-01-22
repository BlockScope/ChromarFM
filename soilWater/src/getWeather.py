#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

t2m = "167.128"
eva = "182.128"
prec = "228.128"
rad = "176.128"

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

def mkEvaReq():
    dreq = defReq()

    dreq["param"] = eva
    dreq["type"] = "fc"
    dreq["time"] = "0000/1200"
    dreq["step"] = "12"
    dreq["target"] = "../data/e_2016.nc"
    dreq["date"] = "20160101/to/20161231"

    return dreq

def mkPrecReq():
    dreq = defReq()

    dreq["param"] = prec
    dreq["type"] = "fc"
    dreq["time"] = "0000/1200"
    dreq["step"] = "12"
    dreq["target"] = "../data/tp_2016.nc"
    dreq["date"] = "20160101/to/20161231"

    return dreq

def mkTempReq():
    dreq = defReq()

    dreq["param"] = t2m
    dreq["time"] = "0000/1200"
    dreq["step"] = "0"
    dreq["target"] = "../data/t2m_2016.nc"
    dreq["date"] = "20160101/to/20161231"

    return dreq

def mkRadReq():
    dreq = defReq()

    dreq["param"] = rad
    dreq["type"] = "fc"
    dreq["time"] = "0000/1200"
    dreq["step"] = "12"

    return dreq

def goTemp():
    server = ECMWFDataServer()
    
    tempReq = mkTempReq()
    tempReq["date"] = "20100101/to/20111231"
    tempReq["target"] = "../data/t2m_20102011.nc"

    
    server.retrieve(tempReq)

def goRad():
    server = ECMWFDataServer()
    radReq = mkRadReq()
    radReq["date"] = "20100101/to/20111231"
    radReq["target"] = "../data/ssr_20102011.nc"

    server.retrieve(radReq)

def go():
    server = ECMWFDataServer()

    evaReq = mkEvaReq()
    precReq = mkPrecReq()

    evaReq["date"] = "20100101/to/20111231"
    evaReq["target"] = "../data/e_20102011.nc"

    precReq["date"] = "20100101/to/20111231"
    precReq["target"] = "../data/tp_20102011.nc"
    
    server.retrieve(evaReq)
    server.retrieve(precReq)
