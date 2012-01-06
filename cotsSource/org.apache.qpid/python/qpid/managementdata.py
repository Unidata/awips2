#!/usr/bin/env python

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#


###############################################################################
## This file is being obsoleted by qmf/console.py
###############################################################################

import qpid
import re
import socket
import struct
import os
import platform
import locale
from qpid.connection import Timeout
from qpid.management import managementChannel, managementClient
from threading       import Lock
from disp            import Display
from shlex           import split
from qpid.connection import Connection
from qpid.util       import connect

class Broker:
  def __init__ (self, text):
    rex = re.compile(r"""
    # [   <user>  [   / <password> ] @]  <host>  [   :<port>   ]
    ^ (?: ([^/]*) (?: / ([^@]*)   )? @)? ([^:]+) (?: :([0-9]+))?$""", re.X)
    match = rex.match(text)
    if not match: raise ValueError("'%s' is not a valid broker url" % (text))
    user, password, host, port = match.groups()

    if port: self.port = int(port)
    else: self.port = 5672
    for addr in socket.getaddrinfo(host, self.port):
      if addr[1] == socket.AF_INET:
        self.host = addr[4][0]
    self.username = user or "guest"
    self.password = password or "guest"

  def name (self):
    return self.host + ":" + str (self.port)

class ManagementData:

  #
  # Data Structure:
  #
  # Please note that this data structure holds only the most recent
  # configuration and instrumentation data for each object.  It does
  # not hold the detailed historical data that is sent from the broker.
  # The only historical data it keeps are the high and low watermarks
  # for hi-lo statistics.
  #
  #    tables        :== {class-key}
  #                        {<obj-id>}
  #                          (timestamp, config-record, inst-record)
  #    class-key     :== (<package-name>, <class-name>, <class-hash>)
  #    timestamp     :== (<last-interval-time>, <create-time>, <delete-time>)
  #    config-record :== [element]
  #    inst-record   :== [element]
  #    element       :== (<element-name>, <element-value>)
  #

  def registerObjId (self, objId):
    if not objId.index() in self.idBackMap:
      self.idBackMap[objId.index()] = self.nextId
      self.idMap[self.nextId] = objId
      self.nextId += 1

  def displayObjId (self, objIdIndex):
    if objIdIndex in self.idBackMap:
      return self.idBackMap[objIdIndex]
    else:
      return 0

  def rawObjId (self, displayId):
    if displayId in self.idMap:
      return self.idMap[displayId]
    else:
      return None

  def displayClassName (self, cls):
    (packageName, className, hash) = cls
    rev = self.schema[cls][4]
    if rev == 0:
      suffix = ""
    else:
      suffix = ".%d" % rev
    return packageName + ":" + className + suffix

  def dataHandler (self, context, className, list, timestamps):
    """ Callback for configuration and instrumentation data updates """
    self.lock.acquire ()
    try:
      # If this class has not been seen before, create an empty dictionary to
      # hold objects of this class
      if className not in self.tables:
        self.tables[className] = {}

      # Register the ID so a more friendly presentation can be displayed
      objId = list[0][1]
      oidx  = objId.index()
      self.registerObjId (objId)

      # If this object hasn't been seen before, create a new object record with
      # the timestamps and empty lists for configuration and instrumentation data.
      if oidx not in self.tables[className]:
        self.tables[className][oidx] = (timestamps, [], [])

      (unused, oldConf, oldInst) = self.tables[className][oidx]

      # For config updates, simply replace old config list with the new one.
      if   context == 0: #config
        self.tables[className][oidx] = (timestamps, list, oldInst)

      # For instrumentation updates, carry the minimum and maximum values for
      # "hi-lo" stats forward.
      elif context == 1: #inst
        if len (oldInst) == 0:
          newInst = list
        else:
          newInst = []
          for idx in range (len (list)):
            (key, value) = list[idx]
            if key.find ("High") == len (key) - 4:
              if oldInst[idx][1] > value:
                value = oldInst[idx][1]
            if key.find ("Low") == len (key) - 3:
              if oldInst[idx][1] < value:
                value = oldInst[idx][1]
            newInst.append ((key, value))
        self.tables[className][oidx] = (timestamps, oldConf, newInst)

    finally:
      self.lock.release ()

  def ctrlHandler (self, context, op, data):
    if op == self.mclient.CTRL_BROKER_INFO:
      pass
    elif op == self.mclient.CTRL_HEARTBEAT:
      pass

  def configHandler (self, context, className, list, timestamps):
    self.dataHandler (0, className, list, timestamps);

  def instHandler (self, context, className, list, timestamps):
    self.dataHandler (1, className, list, timestamps);

  def methodReply (self, broker, sequence, status, sText, args):
    """ Callback for method-reply messages """
    self.lock.acquire ()
    try:
      line = "Call Result: " + self.methodsPending[sequence] + \
             "  " + str (status) + " (" + sText + ")"
      print line, args
      del self.methodsPending[sequence]
    finally:
      self.lock.release ()

  def closeHandler (self, context, reason):
    if self.operational:
      print "Connection to broker lost:", reason
    self.operational = False
    if self.cli != None:
      self.cli.setPromptMessage ("Broker Disconnected")

  def schemaHandler (self, context, classKey, configs, insts, methods, events):
    """ Callback for schema updates """
    if classKey not in self.schema:
      schemaRev = 0
      for key in self.schema:
        if classKey[0] == key[0] and classKey[1] == key[1]:
          schemaRev += 1
      self.schema[classKey] = (configs, insts, methods, events, schemaRev)

  def setCli (self, cliobj):
    self.cli = cliobj

  def __init__ (self, disp, host, username="guest", password="guest"):
    self.lock           = Lock ()
    self.tables         = {}
    self.schema         = {}
    self.bootSequence   = 0
    self.operational    = False
    self.disp           = disp
    self.cli            = None
    self.lastUnit       = None
    self.methodSeq      = 1
    self.methodsPending = {}
    self.sessionId      = "%s.%d" % (platform.uname()[1], os.getpid())

    self.broker = Broker (host)
    sock = connect (self.broker.host, self.broker.port)
    oldTimeout = sock.gettimeout()
    sock.settimeout(10)
    self.conn   = Connection (sock,
                              username=self.broker.username, password=self.broker.password)
    def aborted():
      raise Timeout("Waiting for connection to be established with broker")
    oldAborted = self.conn.aborted
    self.conn.aborted = aborted

    self.conn.start ()

    sock.settimeout(oldTimeout)
    self.conn.aborted = oldAborted

    self.mclient = managementClient ("unused", self.ctrlHandler, self.configHandler,
                                     self.instHandler, self.methodReply, self.closeHandler)
    self.mclient.schemaListener (self.schemaHandler)
    self.mch = self.mclient.addChannel (self.conn.session(self.sessionId))
    self.operational = True
    self.idMap = {}
    self.idBackMap = {}
    self.nextId = 101

  def close (self):
    pass

  def refName (self, oid):
    if oid == None:
      return "NULL"
    return str (self.displayObjId (oid.index()))

  def valueDisplay (self, classKey, key, value):
    if value == None:
      return "<NULL>"
    for kind in range (2):
      schema = self.schema[classKey][kind]
      for item in schema:
        if item[0] == key:
          typecode = item[1]
          unit     = item[2]
          if (typecode >= 1 and typecode <= 5) or typecode == 12 or typecode == 13 or \
                (typecode >= 16 and typecode <= 19):
            if unit == None or unit == self.lastUnit:
              return str (value)
            else:
              self.lastUnit = unit
              suffix = ""
              if value != 1:
                suffix = "s"
              return str (value) + " " + unit + suffix
          elif typecode == 6 or typecode == 7: # strings
            return value
          elif typecode == 8:
            if value == 0:
              return "--"
            return self.disp.timestamp (value)
          elif typecode == 9:
            return str (value)
          elif typecode == 10:
            return self.refName (value)
          elif typecode == 11:
            if value == 0:
              return "False"
            else:
              return "True"
          elif typecode == 14:
            return str (value)
          elif typecode == 15:
            return str (value)
    return "*type-error*"

  def getObjIndex (self, classKey, config):
    """ Concatenate the values from index columns to form a unique object name """
    result = ""
    schemaConfig = self.schema[classKey][0]
    for item in schemaConfig:
      if item[5] == 1 and item[0] != "id":
        if result != "":
          result = result + "."
        for key,val in config:
          if key == item[0]:
            result = result + self.valueDisplay (classKey, key, val)
    return result

  def getClassKey (self, className):
    delimPos = className.find(":")
    if delimPos == -1:
      schemaRev = 0
      delim = className.find(".")
      if delim != -1:
        schemaRev = int(className[delim + 1:])
        name      = className[0:delim]
      else:
        name = className
      for key in self.schema:
        if key[1] == name and self.schema[key][4] == schemaRev:
          return key
    else:
      package   = className[0:delimPos]
      name      = className[delimPos + 1:]
      schemaRev = 0
      delim = name.find(".")
      if delim != -1:
        schemaRev = int(name[delim + 1:])
        name      = name[0:delim]
      for key in self.schema:
        if key[0] == package and key[1] == name:
          if self.schema[key][4] == schemaRev:
            return key
    return None

  def classCompletions (self, prefix):
    """ Provide a list of candidate class names for command completion """
    self.lock.acquire ()
    complist = []
    try:
      for name in self.tables:
        if name.find (prefix) == 0:
          complist.append (name)
    finally:
      self.lock.release ()
    return complist

  def typeName (self, typecode):
    """ Convert type-codes to printable strings """
    if   typecode == 1:
      return "uint8"
    elif typecode == 2:
      return "uint16"
    elif typecode == 3:
      return "uint32"
    elif typecode == 4:
      return "uint64"
    elif typecode == 5:
      return "bool"
    elif typecode == 6:
      return "short-string"
    elif typecode == 7:
      return "long-string"
    elif typecode == 8:
      return "abs-time"
    elif typecode == 9:
      return "delta-time"
    elif typecode == 10:
      return "reference"
    elif typecode == 11:
      return "boolean"
    elif typecode == 12:
      return "float"
    elif typecode == 13:
      return "double"
    elif typecode == 14:
      return "uuid"
    elif typecode == 15:
      return "field-table"
    elif typecode == 16:
      return "int8"
    elif typecode == 17:
      return "int16"
    elif typecode == 18:
      return "int32"
    elif typecode == 19:
      return "int64"
    elif typecode == 20:
      return "object"
    elif typecode == 21:
      return "list"
    elif typecode == 22:
      return "array"      
    else:
      raise ValueError ("Invalid type code: %d" % typecode)

  def accessName (self, code):
    """ Convert element access codes to printable strings """
    if code == 1:
      return "ReadCreate"
    elif code == 2:
      return "ReadWrite"
    elif code == 3:
      return "ReadOnly"
    else:
      raise ValueError ("Invalid access code: %d" %code)

  def notNone (self, text):
    if text == None:
      return ""
    else:
      return text

  def isOid (self, id):
    for char in str (id):
      if not char.isdigit () and not char == '-':
        return False
    return True

  def listOfIds (self, classKey, tokens):
    """ Generate a tuple of object ids for a classname based on command tokens. """
    list = []
    if len(tokens) == 0 or tokens[0] == "all":
      for id in self.tables[classKey]:
        list.append (self.displayObjId (id))

    elif tokens[0] == "active":
      for id in self.tables[classKey]:
        if self.tables[classKey][id][0][2] == 0:
          list.append (self.displayObjId (id))

    else:
      for token in tokens:
        if self.isOid (token):
          if token.find ("-") != -1:
            ids = token.split("-", 2)
            for id in range (int (ids[0]), int (ids[1]) + 1):
              if self.getClassForId (self.rawObjId (long (id))) == classKey:
                list.append (id)
          else:
            list.append (int(token))

    list.sort ()
    result = ()
    for item in list:
      result = result + (item,)
    return result

  def listClasses (self):
    """ Generate a display of the list of classes """
    self.lock.acquire ()
    try:
      rows = []
      sorted = self.tables.keys ()
      sorted.sort ()
      for name in sorted:
        active  = 0
        deleted = 0
        for record in self.tables[name]:
          isdel = False
          ts    = self.tables[name][record][0]
          if ts[2] > 0:
            isdel = True
          if isdel:
            deleted = deleted + 1
          else:
            active = active + 1
        rows.append ((self.displayClassName (name), active, deleted))
      if len (rows) != 0:
        self.disp.table ("Management Object Types:",
                         ("ObjectType", "Active", "Deleted"), rows)
      else:
        print "Waiting for next periodic update"
    finally:
      self.lock.release ()

  def listObjects (self, tokens):
    """ Generate a display of a list of objects in a class """
    if len(tokens) == 0:
      print "Error - No class name provided"
      return

    self.lock.acquire ()
    try:
      classKey = self.getClassKey (tokens[0])
      if classKey == None:
        print ("Object type %s not known" % tokens[0])
      else:
        rows = []
        if classKey in self.tables:
          ids = self.listOfIds(classKey, tokens[1:])
          for objId in ids:
            (ts, config, inst) = self.tables[classKey][self.rawObjId(objId).index()]
            createTime  = self.disp.timestamp (ts[1])
            destroyTime = "-"
            if ts[2] > 0:
              destroyTime = self.disp.timestamp (ts[2])
            objIndex = self.getObjIndex (classKey, config)
            row = (objId, createTime, destroyTime, objIndex)
            rows.append (row)
          self.disp.table ("Objects of type %s" % self.displayClassName(classKey),
                           ("ID", "Created", "Destroyed", "Index"),
                           rows)
    finally:
      self.lock.release ()

  def showObjects (self, tokens):
    """ Generate a display of object data for a particular class """
    self.lock.acquire ()
    try:
      self.lastUnit = None
      if self.isOid (tokens[0]):
        if tokens[0].find ("-") != -1:
          rootId = int (tokens[0][0:tokens[0].find ("-")])
        else:
          rootId = int (tokens[0])

        classKey  = self.getClassForId (self.rawObjId (rootId))
        remaining = tokens
        if classKey == None:
          print "Id not known: %d" % int (tokens[0])
          raise ValueError ()
      else:
        classKey  = self.getClassKey (tokens[0])
        remaining = tokens[1:]
        if classKey not in self.tables:
          print "Class not known: %s" % tokens[0]
          raise ValueError ()

      userIds = self.listOfIds (classKey, remaining)
      if len (userIds) == 0:
        print "No object IDs supplied"
        raise ValueError ()

      ids = []
      for id in userIds:
        if self.getClassForId (self.rawObjId (long (id))) == classKey:
          ids.append (self.rawObjId (long (id)))

      rows = []
      timestamp = None
      config = self.tables[classKey][ids[0].index()][1]
      for eIdx in range (len (config)):
        key = config[eIdx][0]
        if key != "id":
          row   = ("property", key)
          for id in ids:
            if timestamp == None or \
               timestamp < self.tables[classKey][id.index()][0][0]:
              timestamp = self.tables[classKey][id.index()][0][0]
            (key, value) = self.tables[classKey][id.index()][1][eIdx]
            row = row + (self.valueDisplay (classKey, key, value),)
          rows.append (row)

      inst = self.tables[classKey][ids[0].index()][2]
      for eIdx in range (len (inst)):
        key = inst[eIdx][0]
        if key != "id":
          row = ("statistic", key)
          for id in ids:
            (key, value) = self.tables[classKey][id.index()][2][eIdx]
            row = row + (self.valueDisplay (classKey, key, value),)
          rows.append (row)

      titleRow = ("Type", "Element")
      for id in ids:
        titleRow = titleRow + (self.refName(id),)
      caption = "Object of type %s:" % self.displayClassName(classKey)
      if timestamp != None:
        caption = caption + " (last sample time: " + self.disp.timestamp (timestamp) + ")"
      self.disp.table (caption, titleRow, rows)

    except:
      pass
    self.lock.release ()

  def schemaSummary (self):
    """ Generate a display of the list of classes in the schema """
    self.lock.acquire ()
    try:
      rows = []
      sorted = self.schema.keys ()
      sorted.sort ()
      for classKey in sorted:
        tuple = self.schema[classKey]
        row = (self.displayClassName(classKey), len (tuple[0]), len (tuple[1]),
               len (tuple[2]))
        rows.append (row)
      self.disp.table ("Classes in Schema:",
                       ("Class", "Properties", "Statistics", "Methods"),
                       rows)
    finally:
      self.lock.release ()

  def schemaTable (self, className):
    """ Generate a display of details of the schema of a particular class """
    self.lock.acquire ()
    try:
      classKey = self.getClassKey (className)
      if classKey == None:
        print ("Class name %s not known" % className)
        raise ValueError ()

      rows = []
      schemaRev =  self.schema[classKey][4]
      for config in self.schema[classKey][0]:
        name     = config[0]
        if name != "id":
          typename = self.typeName(config[1])
          unit     = self.notNone (config[2])
          desc     = self.notNone (config[3])
          access   = self.accessName (config[4])
          extra    = ""
          if config[5] == 1:
            extra += "index "
          if config[6] != None:
            extra += "Min: " + str(config[6]) + " "
          if config[7] != None:
            extra += "Max: " + str(config[7]) + " "
          if config[8] != None:
            extra += "MaxLen: " + str(config[8]) + " "
          if config[9] == 1:
            extra += "optional "
          rows.append ((name, typename, unit, access, extra, desc))
        
      for config in self.schema[classKey][1]:
        name     = config[0]
        if name != "id":
          typename = self.typeName(config[1])
          unit     = self.notNone (config[2])
          desc     = self.notNone (config[3])
          rows.append ((name, typename, unit, "", "", desc))

      titles = ("Element", "Type", "Unit", "Access", "Notes", "Description")
      self.disp.table ("Schema for class '%s':" % self.displayClassName(classKey), titles, rows)

      for mname in self.schema[classKey][2]:
        (mdesc, args) = self.schema[classKey][2][mname]
        caption = "\nMethod '%s' %s" % (mname, self.notNone (mdesc))
        rows = []
        for arg in args:
          name     = arg[0]
          typename = self.typeName (arg[1])
          dir      = arg[2]
          unit     = self.notNone (arg[3])
          desc     = self.notNone (arg[4])
          extra    = ""
          if arg[5] != None:
            extra = extra + "Min: " + str (arg[5])
          if arg[6] != None:
            extra = extra + "Max: " + str (arg[6])
          if arg[7] != None:
            extra = extra + "MaxLen: " + str (arg[7])
          if arg[8] != None:
            extra = extra + "Default: " + str (arg[8])
          rows.append ((name, typename, dir, unit, extra, desc))
        titles = ("Argument", "Type", "Direction", "Unit", "Notes", "Description")
        self.disp.table (caption, titles, rows)

    except Exception,e:
      pass
    self.lock.release ()

  def getClassForId (self, objId):
    """ Given an object ID, return the class key for the referenced object """
    for classKey in self.tables:
      if objId.index() in self.tables[classKey]:
        return classKey
    return None

  def callMethod (self, userOid, methodName, args):
    self.lock.acquire ()
    methodOk = True
    try:
      classKey = self.getClassForId (self.rawObjId (userOid))
      if classKey == None:
        raise ValueError ()

      if methodName not in self.schema[classKey][2]:
        print "Method '%s' not valid for class '%s'" % (methodName, self.displayClassName(classKey))
        raise ValueError ()

      schemaMethod = self.schema[classKey][2][methodName]
      count = 0
      for arg in range(len(schemaMethod[1])):
        if schemaMethod[1][arg][2].find("I") != -1:
          count += 1
      if len (args) != count:
        print "Wrong number of method args: Need %d, Got %d" % (count, len (args))
        raise ValueError ()

      namedArgs = {}
      idx = 0
      for arg in range(len(schemaMethod[1])):
        if schemaMethod[1][arg][2].find("I") != -1:
          namedArgs[schemaMethod[1][arg][0]] = args[idx]
          idx += 1

      self.methodSeq = self.methodSeq + 1
      self.methodsPending[self.methodSeq] = methodName
    except Exception, e:
      methodOk = False
    self.lock.release ()
    if methodOk:
#      try:
        self.mclient.callMethod (self.mch, self.methodSeq, self.rawObjId (userOid), classKey,
                                 methodName, namedArgs)
#      except ValueError, e:
#        print "Error invoking method:", e

  def makeIdRow (self, displayId):
    if displayId in self.idMap:
      objId = self.idMap[displayId]
    else:
      return None
    if objId.getFlags() == 0:
      flags = ""
    else:
      flags = str(objId.getFlags())
    seq = objId.getSequence()
    if seq == 0:
      seqText = "<durable>"
    else:
      seqText = str(seq)
    return (displayId, flags, seqText, objId.getBroker(), objId.getBank(), hex(objId.getObject()))

  def listIds (self, select):
    rows = []
    if select == 0:
      sorted = self.idMap.keys()
      sorted.sort()
      for displayId in sorted:
        row = self.makeIdRow (displayId)
        rows.append(row)
    else:
      row = self.makeIdRow (select)
      if row == None:
        print "Display Id %d not known" % select
        return
      rows.append(row)
    self.disp.table("Translation of Display IDs:",
                    ("DisplayID", "Flags", "BootSequence", "Broker", "Bank", "Object"),
                    rows)

  def do_list (self, data):
    tokens = data.split ()
    if len (tokens) == 0:
      self.listClasses ()
    else:
      self.listObjects (tokens)

  def do_show (self, data):
    tokens = data.split ()
    self.showObjects (tokens)

  def do_schema (self, data):
    if data == "":
      self.schemaSummary ()
    else:
      self.schemaTable (data)

  def do_call (self, data):
    encTokens = data.split ()
    try:
      tokens = [a.decode(locale.getpreferredencoding()) for a in encArgs]
    except:
      tokens = encTokens
    if len (tokens) < 2:
      print "Not enough arguments supplied"
      return
    
    displayId  = long (tokens[0])
    methodName = tokens[1]
    args       = tokens[2:]
    self.callMethod (displayId, methodName, args)

  def do_id (self, data):
    if data == "":
      select = 0
    else:
      select = int(data)
    self.listIds(select)

  def do_exit (self):
    self.mclient.removeChannel (self.mch)
