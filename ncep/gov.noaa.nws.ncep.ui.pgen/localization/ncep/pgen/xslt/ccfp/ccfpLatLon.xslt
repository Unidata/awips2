<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  	<xsl:output method="text" encoding="UTF-8"/> 
	
	<xsl:template match="DrawableElement/Sigmet/Point">

		<xsl:choose>
  			<xsl:when test="@Lat &gt; 0"><xsl:value-of select="round(10*@Lat)"/></xsl:when>
  			<xsl:otherwise><xsl:value-of select="round(-10*@Lat)"/></xsl:otherwise>
		</xsl:choose>
		<xsl:text>&#xA0;</xsl:text>

		<xsl:choose>
  			<xsl:when test="@Lon &gt; 0"><xsl:value-of select="round(10*@Lon)"/></xsl:when>
  			<xsl:otherwise><xsl:value-of select="round(-10*@Lon)"/></xsl:otherwise>
		</xsl:choose>
		<xsl:text>&#xA0;</xsl:text>	
	       
	</xsl:template>

</xsl:stylesheet> 
