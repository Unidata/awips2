<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
       <!--
        GetAmd
    
        This template checks all the smear elements and outputs a string of 
	AMD (meaning amended) if the Status is CAN (canceled) or NEW 
	(new) or AMD (amended).  If the Status is COR (corrected) then 
            COR is output.  Note that because this checks every smear the output may well 
	be more than a single string.  You will have to trap this output and 
	use the SetAmd template below to get a single AMD in a variable.

        Change Log
        ====== === 
        E. Safford/SAIC        10/05    initial coding
        
    -->
    <xsl:template name="GetAmd">
        <xsl:param name="haz1"></xsl:param>
        <xsl:param name="haz2"></xsl:param>
        <xsl:param name="haz3"></xsl:param>
        
        <xsl:for-each select="//smear">
            <xsl:if test="contains( hazard, '$haz1' )">
                <xsl:choose>
                    <xsl:when test="Status = 'CAN'">
                        AMD
                    </xsl:when>
                    <xsl:when test="Status = 'NEW'">
                        AMD
                    </xsl:when>
                    <xsl:when test="Status = 'AMD'">
                        AMD
                    </xsl:when>
                    <xsl:when test="Status = 'COR'">
                        COR
                    </xsl:when>
                </xsl:choose>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>
