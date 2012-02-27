<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>
    
    <!--
        GetAttentionLine
    
        This template examines the status parameter and writes out one or two lines
        if CAN, NEW, or COR is found in status.  This is to be used for the individual 
        airmet paragraphs which need to include attention lines at the end if
        they are canceled, corrected, or new issuances.

        Change Log
        ====== === 
        E. Safford/SAIC        10/05    initial coding
        E. Safford/SAIC        06/06    add param to handle Outlooks
		E. Safford/SAIC	       08/06    rm CONDS HV ENDED from Outlook line.
        E. Safford/SAIC        06/07    rm "...UPDT TO CANCEL..."        
		B. Yin/SAIC	       	   02/08    add tag number after cancellation 
		B. Yin				   12/11	added new line for 'New' or 'Cor'
    -->
    <xsl:template name="GetAttentionLine">
        <xsl:param name="status"></xsl:param>
        <xsl:param name="airmet_outlook">AIRMET</xsl:param>
        
        <xsl:choose>
            
                    <xsl:when test="contains( $status, 'CAN')">    <!--  Cancel lines -->
                        <xsl:element name="line"><xsl:attribute name="indent">0</xsl:attribute>
                            
                            <xsl:choose>
                                <xsl:when test="contains( $airmet_outlook, 'AIRMET')">
                                    <xsl:text>CANCEL AIRMET. CONDS HV ENDED.</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>CANCEL OUTLOOK.</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            
		    	    <!-- Tag number -->
		            <xsl:variable name="airTag"><xsl:value-of select="airmetTag"/></xsl:variable>
		            <xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if>
            
                        </xsl:element>

                    </xsl:when>


                    <xsl:when test="contains( $status, 'NEW')">    <!--  New line -->
                        <xsl:element name="line"><xsl:attribute name="indent">0</xsl:attribute>
                            
                            <xsl:choose>
                                <xsl:when test="contains( $airmet_outlook, 'AIRMET')">
                                	<xsl:value-of select="$newline"/>
                                    <xsl:text>...NEW AIRMET...</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                	<xsl:value-of select="$newline"/>
                                    <xsl:text>...NEW OUTLOOK...</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            
                        </xsl:element>
                    </xsl:when>
            
                    <xsl:when test="contains( $status, 'COR')">    <!--  Corrected line -->
                        <xsl:element name="line"><xsl:attribute name="indent">0</xsl:attribute>
                            <xsl:choose>
                                <xsl:when test="contains( $airmet_outlook, 'AIRMET')">
                                	<xsl:value-of select="$newline"/>
                                   <xsl:text>...CORRECTED AIRMET...</xsl:text> 
                                </xsl:when>
                                <xsl:otherwise>
                                	<xsl:value-of select="$newline"/>
                                    <xsl:text>...CORRECTED OUTLOOK...</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:element>
                    </xsl:when>
            
            </xsl:choose>
    </xsl:template>
</xsl:stylesheet>
