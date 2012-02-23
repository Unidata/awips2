<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <!-- 
            GetFlightLevel
    
            This template returns a flight level properly prefixed with "FL" if the level is >= 180.
        
            Change Log
            ====== ===
            E. Safford/SAIC            01/06 initial coding
            E. Safford/SAIC            02/06 make "000" -> "SFC"
    -->
    <xsl:template name="GetFlightLevel">
        <xsl:param name="flightLevel"/>
        <xsl:param name="useFL">1</xsl:param>
        
        <xsl:if test="number($flightLevel) >= number('180') ">
            <xsl:if test="number( $useFL ) = number( '1' )">
                <xsl:text>FL</xsl:text>
            </xsl:if>           
        </xsl:if>
        
        <xsl:choose>
            <xsl:when test="number($flightLevel) = number('0') ">
                <xsl:text>SFC</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$flightLevel"/>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    
    <!-- 
        MakeFlightLevel
        
        This template returns a flight level statement for Turb and Ice hazards.
        
        Change Log
        ====== ===
        E. Safford/SAIC            01/06 initial coding
        E. Safford/SAIC            02/06 use FLbase on check for 'SFC' and 'FZL'
    -->
    <xsl:template name="MakeFlightLevel">
        <xsl:param name="base">Missing Base</xsl:param>
        <xsl:param name="top">Missing Top</xsl:param>
        
        <xsl:variable name="FLbase">
            <xsl:call-template name="GetFlightLevel">
                <xsl:with-param name="flightLevel" select="$base"/>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="FLtop">
            <xsl:call-template name="GetFlightLevel">
                <xsl:with-param name="flightLevel" select="$top"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:choose>
            <xsl:when test="contains($FLbase, 'FZL')">
                <xsl:text>BTN FRZLVL AND </xsl:text><xsl:value-of select="$FLtop"/>
            </xsl:when>
            <xsl:when test="contains($FLbase, 'SFC')">
                <xsl:text>BLW </xsl:text><xsl:value-of select="$FLtop"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>BTN </xsl:text><xsl:value-of select="$FLbase"/><xsl:text> AND </xsl:text><xsl:value-of select="$FLtop"/>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>

</xsl:stylesheet>
