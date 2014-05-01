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

<xsl:import href="utils.xsl"/>

<xsl:output indent="yes"/> 

<!-- final preparation of the model -->

<xsl:template match="/">
    <xsl:apply-templates mode="prepare3"/>
</xsl:template>

<xsl:template match="frames" mode="prepare3">
    <frames>
        <xsl:copy-of select="@protocol"/>
        <xsl:apply-templates mode="prepare3"/>
    </frames>
</xsl:template>

<xsl:template match="frame" mode="prepare3">
    <xsl:element name="frame">
        <xsl:copy-of select="@*"/>
	<xsl:if test="field[@type='bit']"><xsl:attribute name="has-bit-field">true</xsl:attribute></xsl:if>
        <xsl:apply-templates mode="prepare3"/>
    </xsl:element>
</xsl:template>


<xsl:template match="field" mode="prepare3">
     <field>
         <xsl:attribute name="type"><xsl:value-of select="@type"/></xsl:attribute>
         <!-- ensure the field name is processed to be a valid java name -->
         <xsl:attribute name="name"><xsl:value-of select="amq:field-name(@name)"/></xsl:attribute>
         <!-- add some attributes to make code generation easier -->
         <xsl:attribute name="csharp-type"><xsl:value-of select="amq:csharp-type(@type)"/></xsl:attribute>
         <xsl:if test="@type='bit'">
             <xsl:attribute name="boolean-index"><xsl:number count="field[@type='bit']"/></xsl:attribute>
         </xsl:if>
     </field>
</xsl:template>

</xsl:stylesheet> 
