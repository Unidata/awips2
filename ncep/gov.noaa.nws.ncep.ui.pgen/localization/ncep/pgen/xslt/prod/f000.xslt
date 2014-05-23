<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text"/>

<xsl:import href="getSfcHighsLows.xslt"/>

<!--
	sfc_prog.xlt
		- generate formatted SFC_PROG text	

	Change Log:

	B. Yin/Chugach	08/11	Initial Coding
	B. Yin/SGT		02/14	Added contour support
-->

<xsl:variable name="newline"><xsl:text>
</xsl:text></xsl:variable>




<xsl:template match="/">

   <xsl:text>12HR PROG VALID xxxxxxZ</xsl:text>
   <xsl:value-of select="$newline"/>
   
<xsl:call-template name="getSfcHighsLows">
	<xsl:with-param name="latFmt" select="'##'"/>
	<xsl:with-param name="lonFmt" select="'##'"/>
</xsl:call-template>
      
<!-- Fronts -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Line">
    	<xsl:if test="@pgenCategory= 'Front'">
   	      <xsl:value-of select="$newline"/>
   		<xsl:choose>
                	<xsl:when test="@pgenType = 'COLD_FRONT'">
                        	<xsl:text>COLD WK </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'WARM_FRONT'">
                        	<xsl:text>WARM WK </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'STATIONARY_FRONT'">
                        	<xsl:text>STNRY WK </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'OCCLUDED_FRONT'">
                        	<xsl:text>OCFNT WK </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'TROF'">
                        	<xsl:text>TROF </xsl:text>
                	</xsl:when>
   		</xsl:choose>
    		<xsl:for-each select="Point">
	      		<xsl:value-of select="format-number(@Lat,'##')"/>
	      		<xsl:value-of select="-1*format-number(@Lon,'##')"/>
              	<xsl:text> </xsl:text>
              	             	
              	<xsl:if test="position() mod 10 = 0 and not(position() = last())">
          			<xsl:value-of select="$newline"/>
				</xsl:if>
    		</xsl:for-each>
        </xsl:if>
    </xsl:for-each>

  </xsl:template>
</xsl:stylesheet>
