<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  	<xsl:output method="text" encoding="UTF-8"/> 

	<xsl:include href="ccfpLatLon.xslt"/>
	
	<xsl:template match="DrawableElement/Sigmet">
	
	<xsl:choose>
		<xsl:when test="@type='Area'">
			<!-- 1st: coverage -->
	      	<xsl:choose>
	        	<xsl:when test="@editableAttrPhenom='75-100%'"><xsl:text>1 </xsl:text></xsl:when>
	        	<xsl:when test="@editableAttrPhenom='40-74%'"><xsl:text>2 </xsl:text></xsl:when>
	        	<xsl:otherwise><xsl:text>3 </xsl:text></xsl:otherwise>
	      	</xsl:choose>

			<!-- 2nd: confidence -->
      		<xsl:choose>
        		<xsl:when test="@editableAttrPhenomLat='50-100%'"><xsl:text>1 </xsl:text></xsl:when>
        		<xsl:otherwise><xsl:text>3 </xsl:text></xsl:otherwise>
      		</xsl:choose>

			<!-- 3rd: growth -->
      		<xsl:choose>
        		<xsl:when test="@editableAttrPhenomLon='+'"><xsl:text>2 </xsl:text></xsl:when>
        		<xsl:when test="@editableAttrPhenomLon='NC'"><xsl:text>3 </xsl:text></xsl:when>
        		<xsl:otherwise><xsl:text>4 </xsl:text></xsl:otherwise>
      		</xsl:choose>

			<!-- 4th: tops -->
      		<xsl:choose>
		        <xsl:when test="@editableAttrPhenom2='400+'"><xsl:text>1 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenom2='350-390'"><xsl:text>2 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenom2='300-340'"><xsl:text>3 </xsl:text></xsl:when>
		        <xsl:otherwise><xsl:text>4 </xsl:text></xsl:otherwise>
      		</xsl:choose>

			<!-- 5th: speed -->
      		<xsl:text><xsl:value-of select="@editableAttrPhenomSpeed"/></xsl:text>

			<!-- 6th: direction -->
	      	<xsl:choose>
		        <xsl:when test="@editableAttrPhenomDirection='N'"><xsl:text> 0 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='NNE'"><xsl:text> 23 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='NE'"><xsl:text> 45 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='ENE'"><xsl:text> 68 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='E'"><xsl:text> 90 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='ESE'"><xsl:text> 113 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='SE'"><xsl:text> 135 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='SSE'"><xsl:text> 158 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='S'"><xsl:text> 180 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='SSW'"><xsl:text> 203 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='SW'"><xsl:text> 225 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='WSW'"><xsl:text> 248 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='W'"><xsl:text> 270 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='WNW'"><xsl:text> 293 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='NW'"><xsl:text> 315 </xsl:text></xsl:when>
		        <xsl:when test="@editableAttrPhenomDirection='NNW'"><xsl:text> 338 </xsl:text></xsl:when>   
		        <xsl:otherwise><xsl:text> 0 </xsl:text></xsl:otherwise>
	      	</xsl:choose>
      
			<!-- 7th: number of points -->
	  		<xsl:value-of select="count(Point)+1"/><xsl:text>&#xA0;</xsl:text>  
	 	</xsl:when>

		<!-- for Line/LineMed -->	 
	 	<xsl:otherwise>
	 
	 		<!-- 1st: Line type -->
	 		<xsl:choose>
	 			<xsl:when test="@type='Line'"><xsl:text>1</xsl:text><xsl:text>&#xA0;</xsl:text></xsl:when>
	 			<xsl:otherwise><xsl:text>2</xsl:text><xsl:text>&#xA0;</xsl:text></xsl:otherwise>	
	 		</xsl:choose> 
	 
	 		<!-- 2nd: number of points -->	
	 	  	<xsl:value-of select="count(Point)"/><xsl:text>&#xA0;</xsl:text>	 
	 	  
	 	</xsl:otherwise>
	 </xsl:choose>    

<!-- rest: latlons -->
	<xsl:for-each select="Point">
	    	<xsl:apply-templates select="."/>			
	</xsl:for-each>

<!-- repeat the first at the end for type Area.  WATCH OUT for minus signs and &lt; &gt; they may cause problems-->
	<xsl:if test="@type='Area'">
		<xsl:choose>
  			<xsl:when test="Point/@Lat &lt; 0"><xsl:value-of select="round(-10*Point/@Lat)"/></xsl:when>
  			<xsl:otherwise><xsl:value-of select="round(10*Point/@Lat)"/></xsl:otherwise>
		</xsl:choose>
		<xsl:text>&#xA0;</xsl:text>
		
		<xsl:choose>
			<xsl:when test="Point/@Lon &lt; 0"><xsl:value-of select="round(-10*Point/@Lon)"/></xsl:when>
  			<xsl:otherwise><xsl:value-of select="round(10*Point/@Lon)"/></xsl:otherwise>
		</xsl:choose>
		<xsl:text>&#xA0;</xsl:text>	
	</xsl:if>	
	       
	</xsl:template>

</xsl:stylesheet> 
