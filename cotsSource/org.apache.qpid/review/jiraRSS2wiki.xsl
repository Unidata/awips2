<?xml version="1.0" encoding="UTF-8"?>
<!--
 -
 - Licensed to the Apache Software Foundation (ASF) under one
 - or more contributor license agreements.  See the NOTICE file
 - distributed with this work for additional information
 - regarding copyright ownership.  The ASF licenses this file
 - to you under the Apache License, Version 2.0 (the
 - "License"); you may not use this file except in compliance
 - with the License.  You may obtain a copy of the License at
 -
 -   http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing,
 - software distributed under the License is distributed on an
 - "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 - KIND, either express or implied.  See the License for the
 - specific language governing permissions and limitations
 - under the License.
 -
 -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="text"></xsl:output>
    <xsl:template match="/">        
        || Key || Component(s) || Affects Version/s|| Summary || Status || Assignee || Reporter || Review Comments ||       
        <xsl:apply-templates select="rss/channel/item"></xsl:apply-templates>
    </xsl:template>
    <xsl:template match="item">
        | [<xsl:value-of select="key"/> |  <![CDATA[https://issues.apache.org/jira/browse/]]><xsl:value-of select="key"/> ] | <xsl:apply-templates select="component"/>  |  <xsl:apply-templates select="version"/> | <xsl:value-of select="title"/> | <xsl:value-of select="status"/> | <xsl:value-of select="assignee"/> | <xsl:value-of select="reporter"/> | |
    </xsl:template>
    <xsl:template match="component">
<xsl:value-of select="."/><xsl:if test="following-sibling::node()[name() = 'component']">, </xsl:if>        
    </xsl:template>
    <xsl:template match="version">
        <xsl:value-of select="."/><xsl:if test="following-sibling::node()[name() = 'version']">, </xsl:if>        
    </xsl:template>
</xsl:stylesheet>
