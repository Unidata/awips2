<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
    
    <!-- 
            OkToUse
    
        This template screens out a list of unwanted phrases from the airmet bulletins.
       
        Change Log
        ====== ===
        E. Safford/SAIC    10/05        initial coding
    -->
    <xsl:template name="OkToUse">
        <xsl:param name="test"/>
        
        <xsl:if test="string-length($test) > 1">
            <xsl:if test="not(contains($test, 'No Qualifier'))"><xsl:value-of select="$test"/></xsl:if>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>
