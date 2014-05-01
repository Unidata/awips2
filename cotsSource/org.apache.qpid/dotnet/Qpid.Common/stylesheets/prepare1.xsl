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
<xsl:param name="asl_base"/>

<!-- pre-process, phase 1 -->

<xsl:template match="/">
    <xsl:apply-templates select="protocol" mode="prepare1"/> 
</xsl:template> 

<xsl:template match="amqp"  mode="prepare1"> 
    <frames>
        <xsl:attribute name="protocol">
            <xsl:value-of select="@comment"/>
            <xsl:text> (</xsl:text>
                <xsl:text>major=</xsl:text><xsl:value-of select="option[@name='protocol_major']/@value"/>
                <xsl:text>, minor=</xsl:text><xsl:value-of select="option[@name='protocol_minor']/@value"/> 
            <xsl:text>)</xsl:text>
        </xsl:attribute>  
        <xsl:apply-templates mode="prepare1" select="inherit"/> 
        <xsl:apply-templates mode="prepare1" select="include"/> 
        <xsl:apply-templates mode="prepare1" select="domain"/> 
        <xsl:apply-templates mode="prepare1" select="class"/> 
    </frames> 
</xsl:template> 

<xsl:template match="include" mode="prepare1"> 
    <xsl:if test="@filename != 'asl_constants.asl'">
        <!-- skip asl_constants.asl, we don't need it and it is not well formed so causes error warnings -->   
        <xsl:apply-templates select="document(@filename)" mode="prepare1"/> 
    </xsl:if> 
</xsl:template> 

<xsl:template match="inherit" mode="prepare1"> 
    <xsl:variable name="ibase" select="concat('file:///', $asl_base, '/', @name, '.asl')"/>
    <xsl:choose>
        <xsl:when test="document($ibase)">  
            <xsl:apply-templates select="document($ibase)" mode="prepare1"/>         
        </xsl:when> 
        <xsl:otherwise>
            <xsl:message>
            Could not inherit from <xsl:value-of select="$ibase"/>; file not found.
            </xsl:message>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template> 

<xsl:template match="class[@index]" mode="prepare1"> 
    <xsl:apply-templates select="method" mode="prepare1"/> 
</xsl:template> 

<xsl:template match="method" mode="prepare1">
    <xsl:if test="parent::class[@index]"><!-- there is a template class that has no index, which we want to skip -->
    <frame>
        <xsl:attribute name="name"><xsl:value-of select="amq:class-name(parent::class/@name, @name)"/></xsl:attribute>
        <xsl:attribute name="class-id"><xsl:value-of select="parent::class/@index"/></xsl:attribute>
        <xsl:if test="@index">
            <xsl:attribute name="method-id"><xsl:value-of select="@index"/></xsl:attribute>
        </xsl:if>
        <xsl:if test="not(@index)">
            <xsl:attribute name="method-id"><xsl:number count="method"/></xsl:attribute>
        </xsl:if>

        <xsl:apply-templates select="field" mode="prepare1"/>
    </frame>
    </xsl:if>
</xsl:template>

<xsl:template match="domain" mode="prepare1"> 
    <domain> 
        <name><xsl:value-of select="@name"/></name> 
        <type><xsl:value-of select="@type"/></type> 
    </domain> 
</xsl:template> 

<xsl:template match="field" mode="prepare1">
    <field>
        <xsl:copy-of select="@name"/>
        <xsl:copy-of select="@type"/>
        <xsl:copy-of select="@domain"/>
    </field>
</xsl:template>

</xsl:stylesheet> 
