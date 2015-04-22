<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
    <!-- 
            OutputHeader
    
        This template produces the three header lines for an an AIRMET bulletin.
       
        Change Log
        ====== ===
        E. Safford/SAIC    10/05        initial coding
    -->
    <xsl:template name="OutputHeader">
        <xsl:param name="report">(missing report)</xsl:param>
        <xsl:param name="hazard">(missing hazard)</xsl:param>
        <xsl:param name="untilTime">(missing untilTime)</xsl:param>
        <xsl:param name="faArea">(missing faArea)</xsl:param>
        <xsl:param name="issueTime">(missing issueTime)</xsl:param>
        <xsl:param name="amend"/>
        
        <xsl:element name="line">
            <xsl:value-of select="$faArea"/><xsl:value-of select="substring($report, 1, 1)"/> WA <xsl:value-of select="$issueTime"/>
            <xsl:if test="string-length($amend) > 1"><xsl:text> </xsl:text><xsl:value-of select="$amend"/></xsl:if>
        </xsl:element>
        
		<xsl:value-of select="$newline"/>
        <xsl:element name="line">AIRMET <xsl:value-of select="$report"/> FOR <xsl:value-of select="$hazard"/> VALID UNTIL <xsl:value-of select="$untilTime"/></xsl:element>
        
    </xsl:template>
</xsl:stylesheet>
