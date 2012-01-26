<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
   
    
    <!--
        GetUpdate
    
        This template examines the status parameter and returns a string of "...UPDT"
        if CAN, AMD, COR, or NEW is found in it.  This is to be used for the individual 
        airmet paragraphs which need to have "...UPDT" included after the state list if
        they are canceled, amended, corrected, or new issuances.

        Change Log
        ====== === 
        E. Safford/SAIC        10/05    initial coding
        
    -->
    <xsl:template name="GetUpdate">
           <xsl:param name="status"/>
        
        <xsl:if test="contains( $status, 'CAN') or contains( $status, 'AMD') or contains( $status, 'COR') or contains($status, 'NEW')" > 
            <xsl:text>...UPDT</xsl:text>
        </xsl:if>
                 
    </xsl:template>
    
</xsl:stylesheet>
