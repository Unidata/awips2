<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
    <xsl:import href="sierra.xsl"/>
    <xsl:import href="tango.xsl"/>
    <xsl:import href="zulu.xsl"/>
    
    <!-- 
    	Product per categoty, per area.
    	Example: category='SIERRA,ZULU'  - might be multiple, using any or no delimiter
    	 		 area='SFO'  - single area here
    		or category='SIERRA'  area='CHI'
    	If no parameters provided, the product is generated for all the categories and all the areas.
    -->
    <xsl:param name="categories">SIERRA TANGO ZULU</xsl:param>
    <xsl:param name="areas">SFO SLC CHI DFW BOS MIA</xsl:param>

	<xsl:output method="text"/>
	
	<xsl:variable name="newline">
<xsl:text>
</xsl:text>
	</xsl:variable>

	<xsl:template match="/">
		<xsl:if test="contains($areas, 'SLC')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">SLC</xsl:with-param></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($areas, 'SFO')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">SFO</xsl:with-param></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($areas, 'CHI')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">CHI</xsl:with-param></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($areas, 'DFW')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">DFW</xsl:with-param></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($areas, 'BOS')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">BOS</xsl:with-param></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($areas, 'MIA')">
			<xsl:call-template name="singleArea"><xsl:with-param name="area">MIA</xsl:with-param></xsl:call-template>
		</xsl:if>
	</xsl:template>
	
	<xsl:template name="singleArea">
		<xsl:param name="area"/>
		
		<xsl:if test="contains($categories, 'SIERRA')">
			<xsl:call-template name="sierra"><xsl:with-param name="area" select="$area"/></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($categories, 'TANGO')">
			<xsl:call-template name="tango"><xsl:with-param name="area" select="$area"/></xsl:call-template>
		</xsl:if>
		<xsl:if test="contains($categories, 'ZULU')">
			<xsl:call-template name="zulu"><xsl:with-param name="area" select="$area"/></xsl:call-template>
		</xsl:if>
		
	</xsl:template>
</xsl:stylesheet>
