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

from time import strftime, gmtime

class Header:
  """ """
  NONE = 1
  KMG = 2
  YN = 3
  Y = 4
  TIME_LONG = 5
  TIME_SHORT = 6
  DURATION = 7

  def __init__(self, text, format=NONE):
    self.text = text
    self.format = format

  def __repr__(self):
    return self.text

  def __str__(self):
    return self.text

  def formatted(self, value):
    try:
      if value == None:
        return ''
      if self.format == Header.NONE:
        return value
      if self.format == Header.KMG:
        return self.num(value)
      if self.format == Header.YN:
        if value:
          return 'Y'
        return 'N'
      if self.format == Header.Y:
        if value:
          return 'Y'
        return ''
      if self.format == Header.TIME_LONG:
         return strftime("%c", gmtime(value / 1000000000))
      if self.format == Header.TIME_SHORT:
         return strftime("%X", gmtime(value / 1000000000))
      if self.format == Header.DURATION:
        if value < 0: value = 0
        sec = value / 1000000000
        min = sec / 60
        hour = min / 60
        day = hour / 24
        result = ""
        if day > 0:
          result = "%dd " % day
        if hour > 0 or result != "":
          result += "%dh " % (hour % 24)
        if min > 0 or result != "":
          result += "%dm " % (min % 60)
        result += "%ds" % (sec % 60)
        return result
    except:
      return "?"

  def numCell(self, value, tag):
    fp = float(value) / 1000.
    if fp < 10.0:
      return "%1.2f%c" % (fp, tag)
    if fp < 100.0:
      return "%2.1f%c" % (fp, tag)
    return "%4d%c" % (value / 1000, tag)

  def num(self, value):
    if value < 1000:
      return "%4d" % value
    if value < 1000000:
      return self.numCell(value, 'k')
    value /= 1000
    if value < 1000000:
      return self.numCell(value, 'm')
    value /= 1000
    return self.numCell(value, 'g')


class Display:
  """ Display formatting for QPID Management CLI """
  
  def __init__(self, spacing=2, prefix="    "):
    self.tableSpacing    = spacing
    self.tablePrefix     = prefix
    self.timestampFormat = "%X"

  def formattedTable(self, title, heads, rows):
    fRows = []
    for row in rows:
      fRow = []
      col = 0
      for cell in row:
        fRow.append(heads[col].formatted(cell))
        col += 1
      fRows.append(fRow)
    headtext = []
    for head in heads:
      headtext.append(head.text)
    self.table(title, headtext, fRows)

  def table(self, title, heads, rows):
    """ Print a table with autosized columns """

    # Pad the rows to the number of heads
    for row in rows:
      diff = len(heads) - len(row)
      for idx in range(diff):
        row.append("")

    print title
    if len (rows) == 0:
      return
    colWidth = []
    col      = 0
    line     = self.tablePrefix
    for head in heads:
      width = len (head)
      for row in rows:
        cellWidth = len (unicode (row[col]))
        if cellWidth > width:
          width = cellWidth
      colWidth.append (width + self.tableSpacing)
      line = line + head
      if col < len (heads) - 1:
        for i in range (colWidth[col] - len (head)):
          line = line + " "
      col = col + 1
    print line
    line = self.tablePrefix
    for width in colWidth:
      for i in range (width):
        line = line + "="
    print line

    for row in rows:
      line = self.tablePrefix
      col  = 0
      for width in colWidth:
        line = line + unicode (row[col])
        if col < len (heads) - 1:
          for i in range (width - len (unicode (row[col]))):
            line = line + " "
        col = col + 1
      print line

  def do_setTimeFormat (self, fmt):
    """ Select timestamp format """
    if fmt == "long":
      self.timestampFormat = "%c"
    elif fmt == "short":
      self.timestampFormat = "%X"

  def timestamp (self, nsec):
    """ Format a nanosecond-since-the-epoch timestamp for printing """
    return strftime (self.timestampFormat, gmtime (nsec / 1000000000))

  def duration(self, nsec):
    if nsec < 0: nsec = 0
    sec = nsec / 1000000000
    min = sec / 60
    hour = min / 60
    day = hour / 24
    result = ""
    if day > 0:
      result = "%dd " % day
    if hour > 0 or result != "":
      result += "%dh " % (hour % 24)
    if min > 0 or result != "":
      result += "%dm " % (min % 60)
    result += "%ds" % (sec % 60)
    return result

class Sortable:
  """ """
  def __init__(self, row, sortIndex):
    self.row = row
    self.sortIndex = sortIndex
    if sortIndex >= len(row):
      raise Exception("sort index exceeds row boundary")

  def __cmp__(self, other):
    return cmp(self.row[self.sortIndex], other.row[self.sortIndex])

  def getRow(self):
    return self.row

class Sorter:
  """ """
  def __init__(self, heads, rows, sortCol, limit=0, inc=True):
    col = 0
    for head in heads:
      if head.text == sortCol:
        break
      col += 1
    if col == len(heads):
      raise Exception("sortCol '%s', not found in headers" % sortCol)

    list = []
    for row in rows:
      list.append(Sortable(row, col))
    list.sort(reverse=not inc)
    count = 0
    self.sorted = []
    for row in list:
      self.sorted.append(row.getRow())
      count += 1
      if count == limit:
        break

  def getSorted(self):
    return self.sorted
