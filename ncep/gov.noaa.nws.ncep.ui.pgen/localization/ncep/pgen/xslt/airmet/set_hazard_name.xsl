<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    
    <!--
        SetHazardName
    
        This template outputs the correct hazard name for the Airmet bulletin. 
	It takes as input the hazard from one smear and outputs the correct
	hazard name.
       
        Change Log
        ====== ===
        E. Safford/SAIC    11/05        initial coding
    -->
    <xsl:template name="SetHazardName">
        <xsl:param name="hazard"/>
        
        <xsl:choose>
            <xsl:when test="contains($hazard, 'TURB')">TURB</xsl:when>
            <xsl:when test="contains($hazard, 'SFC_WND')">STG SFC WNDS</xsl:when>
            <xsl:when test="contains($hazard, 'LLWS')">LLWS</xsl:when>
            <xsl:when test="contains($hazard, 'IFR')">IFR</xsl:when>
            <xsl:when test="contains($hazard, 'MT_OBSC')">MTN OBSCN</xsl:when>
            <xsl:when test="contains($hazard, 'ICE')">ICE</xsl:when>
            <xsl:when test="contains($hazard, 'FZLVL')">FRZLVL</xsl:when>
            <xsl:when test="contains($hazard, 'M_FZLVL')">MULT FRZLVL</xsl:when>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
