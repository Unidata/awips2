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

<xsl:import href="prepare1.xsl"/>
<xsl:import href="prepare2.xsl"/>
<xsl:import href="prepare3.xsl"/>
<xsl:import href="csharp.xsl"/>

<xsl:output indent="yes"/> 
<xsl:output method="text" indent="yes" name="textFormat"/> 

<xsl:template match="/">
    <xsl:variable name="prepare1">
        <xsl:apply-templates mode="prepare1" select="."/>
    </xsl:variable>

    <xsl:variable name="prepare2">
        <xsl:apply-templates mode="prepare2" select="$prepare1"/>
    </xsl:variable>

    <xsl:variable name="model">
        <xsl:apply-templates mode="prepare3" select="$prepare2"/>
    </xsl:variable>

    <xsl:apply-templates mode="generate-multi" select="$model"/>
    <xsl:apply-templates mode="list-registry" select="$model"/>

    <!-- dump out the intermediary files for debugging -->
    <!-- 
    <xsl:result-document href="prepare1.out">
        <xsl:copy-of select="$prepare1"/> 
    </xsl:result-document>

    <xsl:result-document href="prepare2.out">
        <xsl:copy-of select="$prepare2"/> 
    </xsl:result-document>

    <xsl:result-document href="model.out">
        <xsl:copy-of select="$model"/> 
    </xsl:result-document>
    -->
</xsl:template>

</xsl:stylesheet> 
