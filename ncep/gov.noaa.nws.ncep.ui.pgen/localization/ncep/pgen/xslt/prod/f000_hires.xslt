<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text"/>

<xsl:import href="getSfcHighsLows.xslt"/>

<!--
	sfc_prog.xlt
		- generate formatted SFC_PROG text	

	Change Log:

	B. Yin/Chugach	08/11	Initial Coding
-->

<xsl:variable name="newline"><xsl:text>
</xsl:text></xsl:variable>



<xsl:template match="/">

   <xsl:text>12HR PROG VALID xxxxxxZ</xsl:text>
   <xsl:value-of select="$newline"/>
   
<xsl:call-template name="getSfcHighsLows">
	<xsl:with-param name="latFmt" select="'000'"/>
	<xsl:with-param name="lonFmt" select="'0000'"/>
</xsl:call-template>

  <!-- Fronts -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Line">
    	<xsl:if test="@pgenCategory= 'Front'">
   	      <xsl:value-of select="$newline"/>
   		<xsl:choose>
                	<xsl:when test="@pgenType = 'COLD_FRONT'">
                        	<xsl:text>COLD </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'WARM_FRONT'">
                        	<xsl:text>WARM </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'STATIONARY_FRONT'">
                        	<xsl:text>STNRY </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'OCCLUDED_FRONT'">
                        	<xsl:text>OCFNT </xsl:text>
                	</xsl:when>
                	<xsl:when test="@pgenType = 'TROF'">
                        	<xsl:text>TROF </xsl:text>
                	</xsl:when>
   		</xsl:choose>
    		<xsl:for-each select="Point">
	      		<xsl:value-of select="format-number(@Lat*10,'000')"/>
	      		<xsl:value-of select="format-number(@Lon*-10,'0000')"/>
              	<xsl:text> </xsl:text>
              	
              	<xsl:if test="position() mod 7 = 0 and not(position() = last())">
          			<xsl:value-of select="$newline"/>
				</xsl:if>
    		</xsl:for-each>
        </xsl:if>
    </xsl:for-each>

  </xsl:template>
</xsl:stylesheet>
