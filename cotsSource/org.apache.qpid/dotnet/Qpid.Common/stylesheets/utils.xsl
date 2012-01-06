<?xml version='1.0'?>
<!--
 
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements.  See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership.  The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License.  You may obtain a copy of the License at
 
   http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.
 
-->

<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:amq="http://amq.org">

<!-- This file contains functions that are used in the generation of the java classes for framing -->

<!-- retrieve the java type of a given amq type -->
<xsl:function name="amq:csharp-type">
    <xsl:param name="t"/>
    <xsl:choose>
	 <xsl:when test="$t='char'">char</xsl:when> 		 		 
	 <xsl:when test="$t='octet'">byte</xsl:when> 		 		 
	 <xsl:when test="$t='short'">ushort</xsl:when> 		 		 
	 <xsl:when test="$t='shortstr'">string</xsl:when> 		 		 
	 <xsl:when test="$t='longstr'">byte[]</xsl:when> 		 		 
	 <xsl:when test="$t='bit'">bool</xsl:when> 		 		 
	 <xsl:when test="$t='long'">uint</xsl:when> 		 		 
	 <xsl:when test="$t='longlong'">ulong</xsl:when> 		 		 
	 <xsl:when test="$t='table'">FieldTable</xsl:when> 		 		 
         <xsl:otherwise>Object /*WARNING: undefined type*/</xsl:otherwise>
    </xsl:choose>
</xsl:function>

<!-- retrieve the code to get the field size of a given amq type -->
<xsl:function name="amq:field-length">
    <xsl:param name="f"/>
    <xsl:choose>
        <xsl:when test="$f/@type='bit' and $f/@boolean-index=1">
            <xsl:value-of select="concat('1 /*', $f/@name, '*/')"/>
        </xsl:when>
        <xsl:when test="$f/@type='bit' and $f/@boolean-index &gt; 1">
            <xsl:value-of select="concat('0 /*', $f/@name, '*/')"/>
        </xsl:when>
        <xsl:when test="$f/@type='char'">
            <xsl:value-of select="concat('1 /*', $f/@name, '*/')"/>
        </xsl:when>
        <xsl:when test="$f/@type='octet'">
            <xsl:value-of select="concat('1 /*', $f/@name, '*/')"/>
        </xsl:when>
	<xsl:when test="$f/@type='short'">
            <xsl:value-of select="concat('2 /*', $f/@name, '*/')"/>
        </xsl:when>
        <xsl:when test="$f/@type='long'">
            <xsl:value-of select="concat('4 /*', $f/@name, '*/')"/>
        </xsl:when>
        <xsl:when test="$f/@type='longlong'">
            <xsl:value-of select="concat('8 /*', $f/@name, '*/')"/>
        </xsl:when>
	<xsl:when test="$f/@type='shortstr'">
            <xsl:value-of select="concat('(uint)EncodingUtils.EncodedShortStringLength(', $f/@name, ')')"/>
        </xsl:when> 		 		 
	<xsl:when test="$f/@type='longstr'">
            <xsl:value-of select="concat('4 + (uint) (', $f/@name, ' == null ? 0 : ', $f/@name, '.Length)')"/>
        </xsl:when> 		 		 
	<xsl:when test="$f/@type='table'">
            <xsl:value-of select="concat('(uint)EncodingUtils.EncodedFieldTableLength(', $f/@name, ')')"/>
        </xsl:when> 		 		 
        <xsl:otherwise><xsl:text>/* WARNING: COULD NOT DETERMINE FIELD SIZE */</xsl:text></xsl:otherwise>
    </xsl:choose>    
</xsl:function>

<!-- retrieve the code to encode a field of a given amq type -->
<!-- Note:
     This method will not provide an encoder for a bit field. 
     Bit fields should be encoded together separately. -->

<xsl:function name="amq:encoder">
    <xsl:param name="f"/>
    <xsl:choose>
        <xsl:when test="$f/@type='char'">
            <xsl:value-of select="concat('buffer.Put(', $f/@name, ')')"/>
        </xsl:when>
        <xsl:when test="$f/@type='octet'">
            <xsl:value-of select="concat('buffer.Put(', $f/@name, ')')"/>
        </xsl:when>
	<xsl:when test="$f/@type='short'">
            <xsl:value-of select="concat('buffer.Put(', $f/@name, ')')"/>
        </xsl:when>
        <xsl:when test="$f/@type='long'">
            <xsl:value-of select="concat('buffer.Put(', $f/@name, ')')"/>
        </xsl:when>
        <xsl:when test="$f/@type='longlong'">
            <xsl:value-of select="concat('buffer.Put(', $f/@name, ')')"/>
        </xsl:when>
	<xsl:when test="$f/@type='shortstr'">
            <xsl:value-of select="concat('EncodingUtils.WriteShortStringBytes(buffer, ', $f/@name, ')')"/>
        </xsl:when> 		 		 
	<xsl:when test="$f/@type='longstr'">
            <xsl:value-of select="concat('EncodingUtils.WriteLongstr(buffer, ', $f/@name, ')')"/>
        </xsl:when> 		 		 
	<xsl:when test="$f/@type='table'">
            <xsl:value-of select="concat('EncodingUtils.WriteFieldTableBytes(buffer, ', $f/@name, ')')"/>
        </xsl:when> 		 		 
        <xsl:otherwise><xsl:text>/* WARNING: COULD NOT DETERMINE ENCODER */</xsl:text></xsl:otherwise>
    </xsl:choose>    
</xsl:function>

<!-- retrieve the code to decode a field of a given amq type -->
<xsl:function name="amq:decoder">
    <xsl:param name="f"/>
    <xsl:choose>
        <xsl:when test="$f/@type='bit'">
            <xsl:if test="$f/@boolean-index = 1">
                <xsl:text>bool[] bools = EncodingUtils.ReadBooleans(buffer);</xsl:text>
            </xsl:if>
            <xsl:value-of select="concat($f/@name, ' = bools[', $f/@boolean-index - 1 , ']')"/>
        </xsl:when>
        <xsl:when test="$f/@type='char'">
            <xsl:value-of select="concat($f/@name, ' = buffer.GetChar()')"/>
        </xsl:when>
        <xsl:when test="$f/@type='octet'">
            <xsl:value-of select="concat($f/@name, ' = buffer.GetByte()')"/>
        </xsl:when>
        <xsl:when test="$f/@type='short'">
            <xsl:value-of select="concat($f/@name, ' = buffer.GetUInt16()')"/>
        </xsl:when>
        <xsl:when test="$f/@type='long'">
            <xsl:value-of select="concat($f/@name, ' = buffer.GetUInt32()')"/>
        </xsl:when>
        <xsl:when test="$f/@type='longlong'">
            <xsl:value-of select="concat($f/@name, ' = buffer.GetUInt64()')"/>
        </xsl:when>
        <xsl:when test="$f/@type='shortstr'">
            <xsl:value-of select="concat($f/@name, ' = EncodingUtils.ReadShortString(buffer)')"/>
        </xsl:when>
        <xsl:when test="$f/@type='longstr'">
            <xsl:value-of select="concat($f/@name, ' = EncodingUtils.ReadLongstr(buffer)')"/>
        </xsl:when>
        <xsl:when test="$f/@type='table'">
            <xsl:value-of select="concat($f/@name, ' = EncodingUtils.ReadFieldTable(buffer)')"/>
        </xsl:when>
        <xsl:otherwise><xsl:text>/* WARNING: COULD NOT DETERMINE DECODER */</xsl:text></xsl:otherwise>
    </xsl:choose>    
</xsl:function>

<!-- create the class name for a frame, based on class and method (passed in) -->
<xsl:function name="amq:class-name">
    <xsl:param name="class"/>
    <xsl:param name="method"/>
    <xsl:value-of select="concat(amq:upper-first($class),amq:upper-first(amq:field-name($method)), 'Body')"/>
</xsl:function>

<!-- get a valid field name, processing spaces and '-'s where appropriate -->
<xsl:function name="amq:field-name">
    <xsl:param name="name"/>
    <xsl:choose>
        <xsl:when test="contains($name, ' ')">
            <xsl:value-of select="amq:upper-first(concat(substring-before($name, ' '), amq:upper-first(substring-after($name, ' '))))"/>
        </xsl:when>
        <xsl:when test="contains($name, '-')">
            <xsl:value-of select="amq:upper-first(concat(substring-before($name, '-'), amq:upper-first(substring-after($name, '-'))))"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="amq:upper-first($name)"/>
        </xsl:otherwise>
    </xsl:choose>
</xsl:function>

<!-- convert the first character of the input to upper-case -->
<xsl:function name="amq:upper-first">
    <xsl:param name="in"/>
    <xsl:value-of select="concat(upper-case(substring($in, 1, 1)), substring($in, 2))"/>
</xsl:function>

</xsl:stylesheet>
