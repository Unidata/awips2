<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
       <!--
        GetStatus
    
        This template checks all the smear elements and outputs a string of 
	AMD (meaning amended) if the Status is CAN (canceled) or NEW 
	(new) or AMD (amended).  If the Status is COR (corrected) then 
            COR is output.  Note that because this checks every smear the output may well 
	be more than a single string.  You will have to trap this output and then
	use the SetAmd template below to get a single AMD in a variable.

        Change Log
        ====== === 
        E. Safford/SAIC        10/05    initial coding
        E. Safford/SAIC        04/06    add default non-values for haz params
        E. Safford/SAIC        06/06    add consider outlooks for status determination 
		B. Yin/SAIC	       	   08/06	distinguish smear and outlook
		B. Yin/Chugach		   12/11	changed 'Status' to 'issueType'
										changed for-each condition for smears
           
    -->
    <xsl:template name="GetStatus">
        <xsl:param name="haz1">NO HAZARD1</xsl:param>
        <xsl:param name="haz2">NO HAZARD2</xsl:param>
        <xsl:param name="haz3">NO HAZARD3</xsl:param>
        
        <xsl:for-each select="//Gfa[(@hazard = $haz1 or @hazard = $haz2 or @hazard = $haz3) and contains(@fcstHr,'-') and @isOutlook='false']">
        
            <xsl:if test="contains( @hazard, $haz1 ) or contains( @hazard, $haz2 ) or contains( @hazard, $haz3 )">
                <xsl:choose>
                    <xsl:when test="@issueType = 'CAN'">
                        AMD_SMEAR
                    </xsl:when>
                    <xsl:when test="@issueType = 'NEW'">
                        AMD_SMEAR
                    </xsl:when>
                    <xsl:when test="@issueType = 'AMD'">
                        AMD_SMEAR
                    </xsl:when>
                    <xsl:when test="@issueType = 'COR'">
                        COR_SMEAR
                    </xsl:when>
                </xsl:choose>
            </xsl:if>
        </xsl:for-each>
        
        <xsl:for-each select="//outlook">
            <xsl:if test="contains( hazard, $haz1 ) or contains( hazard, $haz2 ) or contains( hazard, $haz3 )">
                <xsl:choose>
                    <xsl:when test="issueType = 'CAN'">
                        AMD_OTLK
                    </xsl:when>
                    <xsl:when test="issueType = 'NEW'">
                        AMD_OTLK
                    </xsl:when>
                    <xsl:when test="issueType = 'AMD'">
                        AMD_OTLK
                    </xsl:when>
                    <xsl:when test="issueType = 'COR'">
                        COR_OTLK
                    </xsl:when>
                </xsl:choose>
            </xsl:if>
        </xsl:for-each>
        
    </xsl:template>
    
    <!--  
        SetStatus
    
        This template takes the amdTest parameter and outputs either AMD 
	(meaning amended) or nothing.  If the calling template traps this 
	output in a variable, then it will have the correct amendement string 
	for use in the Airmet header.

        Change Log
        ====== === 
        E. Safford/SAIC        03/05    initial coding
        E. Safford/SAIC        08/05    add test for COR
	B. Yin/SAIC	       08/06	make smear take precedence over outlook
    -->
    <xsl:template name="SetStatus">
        <xsl:param name="amdTest"/>       
        <xsl:choose>
            <xsl:when test="contains( $amdTest, 'AMD_SMEAR')">AMD</xsl:when>
            <xsl:when test="contains( $amdTest, 'COR_SMEAR')">COR</xsl:when>
            <xsl:when test="contains( $amdTest, 'AMD_OTLK')">AMD</xsl:when>
            <xsl:when test="contains( $amdTest, 'COR_OTLK')">COR</xsl:when>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
