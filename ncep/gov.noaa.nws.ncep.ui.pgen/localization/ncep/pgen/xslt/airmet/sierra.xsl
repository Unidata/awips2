<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:import href="no_hazard_report.xsl"/>
    <xsl:import href="get_status.xsl"/>
    <xsl:import href="get_update.xsl"/>
    <xsl:import href="get_attention_line.xsl"/>
    <xsl:import href="output_header.xsl"/>
    <xsl:import href="output_footer.xsl"/>
    <xsl:import href="ok_to_use.xsl"/>
    <xsl:import href="output_outlook.xsl"/>
    <xsl:import href="set_hazard_name.xsl"/>
    <xsl:import href="make_conditions.xsl"/>
	<xsl:output method="text"/>

	<xsl:variable name="newline">
<xsl:text>
</xsl:text>
	</xsl:variable>
	
	<xsl:template name="sierra">

		<!-- Single area here -->
		<xsl:param name="area"/>

		<xsl:variable name="numSmearIFR" select="count(//Gfa[@hazard='IFR' and contains(@fcstHr,'-') and contains(@area, $area)])"/>
		<xsl:variable name="numSmearMT_OBSC" select="count(//Gfa[@hazard='MT_OBSC' and contains(@fcstHr,'-') and contains(@area, $area)])"/>
		<xsl:variable name="numSmears" select="$numSmearIFR + $numSmearMT_OBSC"/>
	
		<xsl:variable name="numOutlookIFR" select="count(//Gfa[@hazard='IFR' and @isOutlook='true'])" />
		<xsl:variable name="numOutlookMT_OBSC" select="count(//Gfa[@hazard='MT_OBSC' and @isOutlook='true'])" />
		<xsl:variable name="numOutlooks" select="$numOutlookIFR + $numOutlookMT_OBSC" />

		<xsl:variable name="hazardTest">
			<xsl:call-template name="GetHazards"/>
		</xsl:variable>

		<xsl:variable name="untilTime">
			<xsl:call-template name="GetUntilTimeS"/>
		</xsl:variable>

		<xsl:variable name="outlookEndTime">
			<xsl:call-template name="GetOutlookEndTimeS"/>
		</xsl:variable>

		<xsl:variable name="issueTime">
			<xsl:call-template name="GetIssueTimeS"/>
		</xsl:variable>
		
		<xsl:variable name="hazards">
			<xsl:call-template name="SetHazardsS">
				<xsl:with-param name="hazardList" select="$hazardTest"/>
			</xsl:call-template>
		</xsl:variable>

		<xsl:variable name="amdTest">
			<xsl:call-template name="GetStatus">
				<xsl:with-param name="haz1">IFR</xsl:with-param>
				<xsl:with-param name="haz2">MT_OBSC</xsl:with-param>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:variable name="AMD">
			<xsl:call-template name="SetStatus">
				<xsl:with-param name="amdTest" select="$amdTest"/>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:call-template name="OutputHeader">
			<xsl:with-param name="report">SIERRA</xsl:with-param>
			<xsl:with-param name="hazard" select="$hazards"/>
			<xsl:with-param name="untilTime" select="$untilTime"/>
			<xsl:with-param name="faArea" select="$area" />
			<xsl:with-param name="issueTime" select="$issueTime" />
			<xsl:with-param name="amend" select="$AMD"/>
		</xsl:call-template>

		<!--  If no IFR hazards, output no IFR report -->
		<xsl:if test="not($numSmearIFR > 0)">
			<xsl:value-of select="$newline"/>
			<xsl:call-template name="NoHazardReport">
				<xsl:with-param name="expectedHazard">IFR</xsl:with-param>
			</xsl:call-template>
		</xsl:if>
		
		<!--  If any hazards, output all -->
		<xsl:if test="($numSmearIFR > 0) or ($numSmearMT_OBSC > 0)">
			<xsl:call-template name="SierraAirmet">
				<xsl:with-param name="faArea" select="@area"/>
				<xsl:with-param name="issueTime" select="$issueTime"/>
				<xsl:with-param name="untilTime" select="$untilTime"/>
		    </xsl:call-template>
		</xsl:if>

		<!--  output any outlooks -->
		<xsl:if test="$numOutlooks > 0">
			<xsl:call-template name="SierraOutlook">
				<xsl:with-param name="outlookStart" select="$untilTime" />
				<xsl:with-param name="outlookEnd" select="$outlookEndTime" />
				<xsl:with-param name="numIFR" select="$numOutlookIFR" />
				<xsl:with-param name="numMT_OBSC" select="$numOutlookMT_OBSC" />
			</xsl:call-template>
		</xsl:if>
		
		<!-- add bulletin footer -->
		<xsl:call-template name="OutputFooter"/>
			
	</xsl:template>
	
    <xsl:template name="SierraAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

		<xsl:variable name="amdTest">
			<xsl:call-template name="GetStatus">
				<xsl:with-param name="haz1">IFR</xsl:with-param>
				<xsl:with-param name="haz2">MT_OBSC</xsl:with-param>
			</xsl:call-template>
		</xsl:variable>
	
		<xsl:variable name="AMD">
			<xsl:call-template name="SetStatus">
				<xsl:with-param name="amdTest" select="$amdTest" />
			</xsl:call-template>
		</xsl:variable>
	
		<xsl:variable name="hazardTest">
			<xsl:call-template name="GetHazards" />
		</xsl:variable>
        
        <xsl:variable name="hazards">
            <xsl:call-template name="SetHazardsS">
                <xsl:with-param name="hazardList" select="$hazardTest"/>
            </xsl:call-template>
        </xsl:variable>
 	
        <!-- output Sierra Airmet paragraphs -->
		<xsl:call-template name="OutputSierra">
		    <xsl:with-param name="hazType">IFR</xsl:with-param>
		    <xsl:with-param name="faArea" select="$faArea"/>
		</xsl:call-template>
	
		<xsl:call-template name="OutputSierra">
		    <xsl:with-param name="hazType">MT_OBSC</xsl:with-param>
		    <xsl:with-param name="faArea" select="$faArea"/>
		</xsl:call-template>

    </xsl:template>
	
    <xsl:template name="GetHazards">
        <xsl:for-each select="//Gfa">
            <xsl:value-of select="@hazard"/>
        </xsl:for-each>
    </xsl:template>
	
	
	<xsl:template name="GetUntilTimeS">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='IFR' or @hazard='MT_OBSC') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@untilTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>
	
	<xsl:template name="GetOutlookEndTimeS">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='IFR' or @hazard='MT_OBSC') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@outlookEndTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>
	
	<xsl:template name="GetIssueTimeS">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='IFR' or @hazard='MT_OBSC') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@issueTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>

    <xsl:template name="SetHazardsS">
        <xsl:param name="hazardList"/>
        
        <xsl:choose>
            <xsl:when test="contains($hazardList, 'MT_OBSC')">IFR AND MTN OBSCN</xsl:when>
            <xsl:otherwise>IFR</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="OutputSierra">
        <xsl:param name="hazType"/>
        <xsl:param name="faArea"/>

        <xsl:for-each select="//Gfa[@hazard = $hazType and contains(@fcstHr,'-') and @isOutlook='false' and contains(@area, $faArea)]">
            
                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                          <xsl:with-param name="hazard" select="$hazType"/>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="condStatement">
                    <xsl:call-template name="MakeConditionsStatement">
                        <xsl:with-param name="isSmear">1</xsl:with-param>
                        <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="@fromCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="fromCondsEndg"><xsl:value-of select="@fromCondsEndg"/></xsl:with-param>
                        <xsl:with-param name="condsContg"><xsl:value-of select="@condsContg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="@otlkCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="@otlkCondsEndg"/></xsl:with-param>
                    </xsl:call-template>          
                </xsl:variable>

                <xsl:variable name="freqSevStatement">
                	<xsl:call-template name="GetFreqStatement">
                		<xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
                		<xsl:with-param name="hazard" select="@hazard"/>
                		<xsl:with-param name="topBottom"><xsl:value-of select="@topBottom"/></xsl:with-param>
                		<xsl:with-param name="dueTo"><xsl:value-of select="@type"/></xsl:with-param>
                		<xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                	</xsl:call-template>
                </xsl:variable>

                <xsl:variable name="updateFlag">
                    <xsl:call-template name="GetUpdate">
                        <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:value-of select="$newline"/>
                <xsl:text>.</xsl:text>
                
                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/>AIRMET <xsl:value-of select="$hazardName"/>...<xsl:value-of select="@states"/>
                    <xsl:if test="number( string-length($updateFlag) ) > number( '1' )"> 
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                   </xsl:if>    
                </xsl:element>
                
                <!--  From line -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/>FROM <xsl:value-of select="normalize-space(@textVor)"/></xsl:element>
                
                <xsl:variable name="airTag"><xsl:value-of select="@airmetTag"/></xsl:variable>
                
                <!-- Frequency & Severity line -->
                <xsl:if test="not( contains(@issueType, 'CAN' )) and string-length($freqSevStatement) > 1">
                    <xsl:element name="line">
                        <xsl:value-of select="$newline"/><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                 </xsl:if>
    
                <!--  Add the attention line(s) -->
                <xsl:call-template name="GetAttentionLine">
                    <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
                </xsl:call-template>
                
        </xsl:for-each>

    </xsl:template>
    
    <xsl:template name="GetFreqStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
    	<xsl:param name="topBottom"/>
    	<xsl:param name="dueTo"/>
    	<xsl:param name="conditions"/>
    	
    	<xsl:if test="string-length($frequency) >1 or
    		          string-length($topBottom) > 1 or
    		          string-length($dueTo) > 1" >
    	    
    	    <xsl:if test="string-length($frequency) >1">
    	        <xsl:variable name="validFrequency">
    	            
    	            <xsl:call-template name="OkToUse">
    	                <xsl:with-param name="test" select="$frequency"/>
    	            </xsl:call-template>
    	        </xsl:variable>
    	       
    	        <xsl:if test="string-length($validFrequency) > 1">
    	            <xsl:value-of select="normalize-space($validFrequency)"/>
    	        </xsl:if>
    	    </xsl:if>
    	    
    	    <xsl:if test="string-length($topBottom) > 1">
    	        <xsl:text> </xsl:text>
    	        <xsl:value-of select="normalize-space($topBottom)"/>
    	    </xsl:if>
	    
    	    <xsl:if test="string-length($dueTo) > 1">
    	        <xsl:text> </xsl:text>
    	        <xsl:value-of select="normalize-space($dueTo)"/>
    	    </xsl:if> 
               <xsl:text>.</xsl:text>
    	    
        </xsl:if>
    	
        <xsl:if test="string-length( $conditions ) > 1">
            <xsl:text> </xsl:text><xsl:value-of select="normalize-space($conditions)"/>
        </xsl:if>

    </xsl:template>

    <!--  
		SierraOutlook
		
		This template outputs the Sierra Outlooks in the proper order of IFR then MT_OBSC.
		
		Change Log
		====== ===
		E. Safford/SAIC    11/05        initial coding
		E. Safford/SAIC    01/06        add stateList param to OutputOutlook
		E. Safford/SAIC    02/06        add outlookStart/End to OutputOutlook
		E. Safford/SAIC    02/06        fix state list in MT_OBSC outlook
		E. Safford/SAIC    02/06        fix MT_OBSC hazard name
		E. Safford/SAIC    04/06        use MakeConditionsStatement
    -->    
    <xsl:template name="SierraOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numIFR">0</xsl:param>
        <xsl:param name="numMT_OBSC">0</xsl:param>
        
        
        <xsl:variable name="numOutlooks"><xsl:value-of select="number($numIFR + $numMT_OBSC)"/></xsl:variable>
       
 
        <xsl:for-each select="//Gfa[@hazard='IFR' and @isOutlook='true']">
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard"><xsl:value-of select="@hazard"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="condStatement">
                <xsl:call-template name="MakeConditionsStatement">
                    <xsl:with-param name="isSmear">0</xsl:with-param>
                    <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="@fromCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="fromCondsEndg"><xsl:value-of select="@fromCondsEndg"/></xsl:with-param>
                    <xsl:with-param name="condsContg"><xsl:value-of select="@condsContg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="@otlkCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="@otlkCondsEndg"/></xsl:with-param>
                </xsl:call-template>          
            </xsl:variable>
            
            <xsl:variable name="freqSevStatement">
                <xsl:call-template name="GetFreqStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="@hazard"/>
                    <xsl:with-param name="topBottom"><xsl:value-of select="@topBottom"/></xsl:with-param>
                    <xsl:with-param name="dueTo"><xsl:value-of select="@type"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart"><xsl:value-of select="$outlookStart"/></xsl:with-param>
                <xsl:with-param name="outlookEnd"><xsl:value-of select="$outlookEnd"/></xsl:with-param>
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="@states"/>
                <xsl:with-param name="outlookNumber" select="position()"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="@textVor"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>                
            </xsl:call-template>
            
        </xsl:for-each>
        
        <xsl:for-each select="//Gfa[@hazard='MT_OBSC' and @isOutlook='true']">
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard"><xsl:value-of select="@hazard"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="condStatement">
                <xsl:call-template name="MakeConditionsStatement">
                    <xsl:with-param name="isSmear">0</xsl:with-param>
                    <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="@fromCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="fromCondsEndg"><xsl:value-of select="@fromCondsEndg"/></xsl:with-param>
                    <xsl:with-param name="condsContg"><xsl:value-of select="@condsContg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="@otlkCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="@otlkCondsEndg"/></xsl:with-param>
                </xsl:call-template>          
            </xsl:variable>
            
            <xsl:variable name="freqSevStatement">
                <xsl:call-template name="GetFreqStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
                    <xsl:with-param name="hazard"><xsl:value-of select="$hazardName"/></xsl:with-param>
                    <xsl:with-param name="topBottom"><xsl:value-of select="@topBottom"/></xsl:with-param>
                    <xsl:with-param name="dueTo"><xsl:value-of select="@type"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart"><xsl:value-of select="$outlookStart"/></xsl:with-param>
                <xsl:with-param name="outlookEnd"><xsl:value-of select="$outlookEnd"/></xsl:with-param>
                <xsl:with-param name="hazard"><xsl:value-of select="$hazardName"/></xsl:with-param>
                <xsl:with-param name="stateList" select="@states"/>
                <xsl:with-param name="outlookNumber" select="position()+$numIFR"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="@textVor"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>  
            </xsl:call-template>
 
        </xsl:for-each>
        
    </xsl:template>

</xsl:stylesheet>
