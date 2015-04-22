<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
    <!-- 
            NoHazardReport
    
        This template produces an AIRMET report when there are no
        hazards for the specified FA Area.  The template can conditionally omit the footer
        portion of the report (default is to include it).
       
        Change Log
        ====== ===
        E. Safford/SAIC    10/05        initial coding
        E. Safford/SAIC    10/05        rm header & footer creation, rm unused params
    -->
    <xsl:template name="NoHazardReport">
        <xsl:param name="expectedHazard">(missing expectedHazard)</xsl:param>
        
        <xsl:element name="line">.</xsl:element>
        
        <xsl:element name="line">
            <xsl:value-of select="$newline"/>NO SGFNT <xsl:value-of select="$expectedHazard"/> EXP OUTSIDE OF CNVTV ACT.</xsl:element>
        
    </xsl:template>
    
    
</xsl:stylesheet>
