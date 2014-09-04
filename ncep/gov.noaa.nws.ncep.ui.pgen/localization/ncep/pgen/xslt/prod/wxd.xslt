<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text"/>

<!--
	wxd.xslt
		- generate formatted SFC_PROG text	

	Change Log:

	B. Yin/Chugach	08/11	Initial Coding
	P.Swamy         05/2014 TTR 994 - WxD Surface Analysis text product missing a field
-->

<xsl:variable name="newline"><xsl:text>
</xsl:text></xsl:variable>

<xsl:template match="/">

   <xsl:text>yyyymmddhh</xsl:text>

    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Symbol">
   	<xsl:choose>
               	<xsl:when test="@pgenType = 'TROPICAL_STORM_NH'">
   			<xsl:value-of select="$newline"/>
                       	<xsl:text>TROP &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;1</xsl:text>
			<xsl:value-of select="$newline"/>
   			<xsl:text>  </xsl:text>
	      		<xsl:value-of select="format-number(Point/@Lat*100,'##')"/>
   			<xsl:text>  </xsl:text>
	      		<xsl:value-of select="format-number(Point/@Lon*-100,'##')"/>
               	</xsl:when>
               	<xsl:when test="@pgenType = 'HURRICANE_NH'">
   			<xsl:value-of select="$newline"/>
                       	<xsl:text>HRCN &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;1</xsl:text>
			<xsl:value-of select="$newline"/>
   			<xsl:text>  </xsl:text>
	      		<xsl:value-of select="format-number(Point/@Lat*100,'##')"/>
   			<xsl:text>  </xsl:text>
	      		<xsl:value-of select="format-number(Point/@Lon*-100,'##')"/>
               	</xsl:when>
        </xsl:choose>

    </xsl:for-each>

    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Line">
    	<xsl:if test="@pgenCategory= 'Front'">
   	      <xsl:value-of select="$newline"/>
   		<xsl:choose>
                	<xsl:when test="@pgenType = 'COLD_FRONT'">
                            <xsl:text>COLD WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'COLD_FRONT_FORM'">
                            <xsl:text>COLD WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                            <xsl:text>5</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'COLD_FRONT_DISS'">
                            <xsl:text>COLD WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                 			  </xsl:call-template>                            
                			<xsl:text>8</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>                    
                    <xsl:when test="@pgenType = 'WARM_FRONT'">
                            <xsl:text>WARM WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'WARM_FRONT_FORM'">
                            <xsl:text>WARM WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                            <xsl:text>5</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'WARM_FRONT_DISS'">
                            <xsl:text>WARM WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                 			  </xsl:call-template>                            
                			<xsl:text>8</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                                    	                                	                	                	
                    <xsl:when test="@pgenType = 'STATIONARY_FRONT'">
                            <xsl:text>STNRY WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'STATIONARY_FRONT_FORM'">
                            <xsl:text>STNRY WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                            <xsl:text>5</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'STATIONARY_FRONT_DISS'">
                            <xsl:text>STNRY WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                 			  </xsl:call-template>                            
                			<xsl:text>8</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>                	                	
                    <xsl:when test="@pgenType = 'OCCLUDED_FRONT'">
                            <xsl:text>OCFNT WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'OCCLUDED_FRONT_FORM'">
                            <xsl:text>OCFNT WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                			  </xsl:call-template>                            
                            <xsl:text>5</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'OCCLUDED_FRONT_DISS'">
                            <xsl:text>OCFNT WK &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                 			  </xsl:call-template>                            
                			<xsl:text>8</xsl:text>                               
                              <xsl:call-template name="getPoints">
                			  </xsl:call-template>
                    </xsl:when>                	                	                	                	                                	                	                	                	                	                	
                	<xsl:when test="@pgenType = 'TROF' or @pgenType = 'TROPICAL_TROF'">
                        	<xsl:text>TROF &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                        	  <xsl:call-template name="getVerticesAndStrength">
                       		  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                        	  </xsl:call-template>                        	
                	</xsl:when>                       	                	                	         	
                    <xsl:when test="@pgenType = 'DRY_LINE'">
                            <xsl:text>DRYLINE &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                       		  </xsl:call-template>                            
                		    <xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                        	  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'INSTABILITY'">
                            <xsl:text>SQLN &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                       		  </xsl:call-template>                            
                			<xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                        	  </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="@pgenType = 'SHEAR_LINE'">
                            <xsl:text>SHEARLINE &#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;</xsl:text>
                              <xsl:call-template name="getVerticesAndStrength">
                     		  </xsl:call-template>
 			                <xsl:text>0</xsl:text>                               
                              <xsl:call-template name="getPoints">
                        	  </xsl:call-template>
                    </xsl:when>                                       	                	                	                	                	
   		</xsl:choose>    		
        </xsl:if>
    </xsl:for-each>
   <xsl:value-of select="$newline"/>
   <xsl:text>END</xsl:text>

  </xsl:template>
  
  <xsl:template name="getPoints" match="/Products/Product/Layer/DrawableElement/Line">
      <xsl:for-each select="Point">
               <xsl:value-of select="$newline"/>
               <xsl:text>  </xsl:text>
                  <xsl:value-of select="format-number(@Lat*100,'##')"/>
               <xsl:text>  </xsl:text>
                  <xsl:value-of select="format-number(@Lon*-100,'##')"/>
      </xsl:for-each>
  </xsl:template>  
  
  <xsl:template name="getVerticesAndStrength" match="/Products/Product/Layer/DrawableElement/Line">
              <xsl:value-of select="count(Point)" />
            <xsl:text> </xsl:text>
            <xsl:value-of select="substring-before(@lineWidth, '.')" />
                    <xsl:text> </xsl:text>
  </xsl:template>
  
  
</xsl:stylesheet>
