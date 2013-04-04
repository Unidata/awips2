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
    <xsl:import href="make_flight_level.xsl"/>
    <xsl:import href="make_conditions.xsl"/>
	<xsl:output method="text"/>

	<xsl:variable name="newline">
<xsl:text>
</xsl:text>
	</xsl:variable>
	
	<xsl:variable name="numSmearICE" select="count(//Gfa[@hazard='ICE' and contains(@fcstHr,'-')])"/>
	<xsl:variable name="numSmears" select="$numSmearICE"/>
	
	<xsl:variable name="numOutlookICE" select="count(//Gfa[@hazard='ICE' and @isOutlook='true'])"/>
	<xsl:variable name="numOutlooks" select="$numOutlookICE"/>
	
	<xsl:variable name="numFzlvl" select="count(//Gfa[@hazard='FZLVL'])"/>
	<xsl:variable name="numM_Fzlvl" select="count(//Gfa[@hazard='M_FZLVL' and contains(@fcstHr,'-')])"/>
	<xsl:variable name="numFreezing" select="$numFzlvl + $numM_Fzlvl"/>


	<xsl:template name="zulu">

		<xsl:param name="area"/>

		<xsl:variable name="untilTime">
			<xsl:call-template name="GetUntilTimeZ"/>
		</xsl:variable>

		<xsl:variable name="outlookEndTime">
			<xsl:call-template name="GetOutlookEndTimeZ"/>
		</xsl:variable>
		
		<xsl:variable name="issueTime">
			<xsl:call-template name="GetIssueTimeZ"/>
		</xsl:variable>

		<xsl:variable name="amdTest">
			<xsl:call-template name="GetStatus">
				<xsl:with-param name="haz1">ICE</xsl:with-param>
				<xsl:with-param name="haz2">FZLVL</xsl:with-param>
				<xsl:with-param name="haz3">M_FZLVL</xsl:with-param>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:variable name="AMD">
			<xsl:call-template name="SetStatus">
				<xsl:with-param name="amdTest" select="$amdTest"/>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:call-template name="OutputHeader">
			<xsl:with-param name="report">ZULU</xsl:with-param>
			<xsl:with-param name="hazard">ICE AND FRZLVL</xsl:with-param>
			<xsl:with-param name="untilTime" select="$untilTime"/>
			<xsl:with-param name="faArea" select="$area" />
			<xsl:with-param name="issueTime" select="$issueTime" />
			<xsl:with-param name="amend" select="$AMD"/>
		</xsl:call-template>

		<!--  If no ICE hazards, output no hazards report -->
		<xsl:if test="not($numSmearICE > 0)">
			<xsl:value-of select="$newline"/>
			<xsl:call-template name="NoHazardReport">
				<xsl:with-param name="expectedHazard">ICE</xsl:with-param>
			</xsl:call-template>
		</xsl:if>
		
		<!--   If hazards are found output them -->
		<xsl:if test="$numSmears > 0">
			<xsl:call-template name="ZuluAirmet">
				<xsl:with-param name="faArea" select="$area"/>
				<xsl:with-param name="issueTime" select="$issueTime"/>
				<xsl:with-param name="untilTime" select="$untilTime"/>
			</xsl:call-template>
		</xsl:if>

		<!--  output any outlooks -->
		<xsl:if test="$numOutlooks > 0">
			<xsl:call-template name="ZuluOutlook">
				<xsl:with-param name="outlookStart" select="$untilTime"/>
				<xsl:with-param name="outlookEnd" select="$outlookEndTime"/>
				<xsl:with-param name="numICE" select="$numOutlookICE"/>
				<xsl:with-param name="numOutlooks" select="$numOutlooks"/>
			</xsl:call-template>
		</xsl:if>
		
		<!--  add freezing paragraph  -->
		<xsl:choose>
		<xsl:when test="$numFreezing > 0">
		        
		        <xsl:variable name="FRbase">
	<xsl:call-template name="readFzlvlBase">
        	<xsl:with-param name="FAarea" select="$area"/>
        </xsl:call-template>
		        </xsl:variable>
		        
		        <xsl:variable name="FRtop">
	<xsl:call-template name="readFzlvlTop">
        	<xsl:with-param name="FAarea" select="$area"/>
        </xsl:call-template>
		        </xsl:variable>
		        
		        <xsl:call-template name="ZuluFreezing">
		            <xsl:with-param name="FRbase" select="$FRbase"/>
		            <xsl:with-param name="FRtop" select="$FRtop"/> 
		            <xsl:with-param name="ZuluArea" select="$area"/> 
		        </xsl:call-template>
		    </xsl:when>
		    <xsl:otherwise>    <!-- if no freezingRange element exists then just add a placeholder line -->
		        <xsl:element name="line">
		            <xsl:value-of select="$newline"/>.</xsl:element>
		        <xsl:element name="line">
		            <xsl:value-of select="$newline"/>FRZLVL...</xsl:element>
		    </xsl:otherwise>
		</xsl:choose>

		<!-- add bulletin footer -->
		<xsl:call-template name="OutputFooter"/>
			
	</xsl:template>
	
	<xsl:template name="GetUntilTimeZ">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='ICE' or contains(@hazard,'FZLVL')) and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@untilTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
		<xsl:if test="string-length($temp)=0">
			<xsl:variable name="temp2">
				<xsl:for-each select="//Gfa[contains(@fcstHr,'-') and string-length(@untilTime)>0][last()]">
					<xsl:value-of select="@untilTime"/>
				</xsl:for-each>
			</xsl:variable>
			<xsl:value-of select="$temp2"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template name="GetOutlookEndTimeZ">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='ICE' or contains(@hazard,'FZLVL')) and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@outlookEndTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
		<xsl:if test="string-length($temp)=0">
			<xsl:variable name="temp2">
				<xsl:for-each select="//Gfa[contains(@fcstHr,'-') and string-length(@untilTime)>0][last()]">
					<xsl:value-of select="@outlookEndTime"/>
				</xsl:for-each>
			</xsl:variable>
			<xsl:value-of select="$temp2"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template name="GetIssueTimeZ">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='ICE' or contains(@hazard,'FZLVL')) and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@issueTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
		<xsl:if test="string-length($temp)=0">
			<xsl:variable name="temp2">
				<xsl:for-each select="//Gfa[contains(@fcstHr,'-') and string-length(@issueTime)>0][last()]">
					<xsl:value-of select="@issueTime"/>
				</xsl:for-each>
			</xsl:variable>
			<xsl:value-of select="$temp2"/>
		</xsl:if>
	</xsl:template>
	
    <!--  
        ZuluAirmet
    
        This template formats the Zulu Airmet.  Note that the formatting lines 
	begin at the left margin.  

        Change Log
        ====== === 
        E. Safford/SAIC        03/05    initial coding
        E. Safford/SAIC        08/05    Add "...UPDT" to the state list following amended and corrected hazards
        E. Safford/SAIC        10/05    rm header and footer creation
        E. Safford/SAIC        01/06    use Base and Top, not Top_Bottom
        E. Safford/SAIC        02/06    no freq/sev/conds line (line 3) for CAN airmet
        E. Safford/SAIC        04/06    use MakeConditionsStatement
        E. Safford/SAIC        05/06    add params to GetZuluFreqSevStatement
	B. Yin/SAIC	       07/07	add tag number
        B. Yin/SAIC            02/08    removed tag number for cancellation (added into attention line)
    -->    
    <xsl:template name="ZuluAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">ICE</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>
         
        <!--  
	    output Zulu Airmets 
		-->
        <xsl:for-each select="//Gfa[@hazard='ICE' and contains(@fcstHr,'-') and @isOutlook='false']">
            
            <xsl:if test="@hazard = 'ICE'">
                
                <xsl:variable name="status"><xsl:value-of select="@issueType"/></xsl:variable>
                
                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                          <xsl:with-param name="hazard" select="@hazard"/>
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
                	<xsl:call-template name="GetZuluFreqSevStatement">
						<xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
						<xsl:with-param name="severity"><xsl:value-of select="@type"/></xsl:with-param>
						<xsl:with-param name="hazard" select="@hazard"/>
						<xsl:with-param name="base" select="@bottom"/>
						<xsl:with-param name="top" select="@top"/>
						<xsl:with-param name="fzlBase" select="@fzlBase"/>
						<xsl:with-param name="fzlTop" select="@fzlTop"/>     
						<xsl:with-param name="dueTo"><xsl:value-of select="@dueTo"/></xsl:with-param>
						<xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                	</xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="updateFlag">
                    <xsl:call-template name="GetUpdate">
                        <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <!-- 
                        Output begins here
                -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/>.</xsl:element>
                <xsl:text>
                </xsl:text>

                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/>AIRMET <xsl:value-of select="$hazardName"/>...<xsl:value-of select="normalize-space(@states)"/>
                    <xsl:if test="string-length($updateFlag) > 1">
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                    </xsl:if>
                </xsl:element>
                
                 
                <!-- From line -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/>FROM <xsl:value-of select="normalize-space(@textVor)"/></xsl:element>

                <!-- Frequency & Severity line -->
                <xsl:variable name="airTag"><xsl:value-of select="@airmetTag"/></xsl:variable>

                <xsl:if test="not( contains( $status, 'CAN' ))">
                    <xsl:if test="string-length($freqSevStatement) > 1">
                        <xsl:element name="line">
                            <xsl:value-of select="$newline"/><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                    </xsl:if>
                </xsl:if>

                <!--  Add the attention line(s) -->
                <xsl:call-template name="GetAttentionLine">
                    <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                </xsl:call-template>  
                
            </xsl:if>
        </xsl:for-each>
        
    </xsl:template>

    <xsl:template name="ZuluOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numICE">0</xsl:param>
        <xsl:param name="numOutlooks">0</xsl:param>


        <xsl:for-each select="//Gfa[@hazard='ICE' and @isOutlook='true']">
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
                <xsl:call-template name="GetZuluFreqSevStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
                    <xsl:with-param name="severity"><xsl:value-of select="@type"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="@hazard"/>
                    <xsl:with-param name="base" select="@bottom"/>
                    <xsl:with-param name="top" select="@top"/>
                    <xsl:with-param name="fzlBase" select="@fzlBase"/>
                    <xsl:with-param name="fzlTop" select="@fzlTop"/>     
                    <xsl:with-param name="dueTo"><xsl:value-of select="@dueTo"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart" select="$outlookStart"/>
                <xsl:with-param name="outlookEnd" select="$outlookEnd"/>
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="@states"/>
                <xsl:with-param name="outlookNumber" select="position()"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="@textVor"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>                
            </xsl:call-template>
            
        </xsl:for-each>
        
    </xsl:template>    

    <xsl:template name="ZuluFreezing">
        <xsl:param name="FRbase">MissingBase</xsl:param>
        <xsl:param name="FRtop">MissingTop</xsl:param>
        <xsl:param name="ZuluArea">Missingarea</xsl:param>
        
        <xsl:element name="line">
            <xsl:value-of select="$newline"/><xsl:text>.</xsl:text>
        </xsl:element>
        
        <xsl:element name="line">
            <xsl:value-of select="$newline"/>
            <xsl:text>FRZLVL...</xsl:text>

            <xsl:if test="not( contains( $FRtop, 'SFC' ))">
                <xsl:text>RANGING FROM </xsl:text>
            </xsl:if>
 
            <xsl:value-of select="$FRbase"/>
           
            <xsl:if test="not( contains( $FRtop, 'SFC' ))">
                <xsl:text>-</xsl:text> 
                <xsl:value-of select="$FRtop"/>
            </xsl:if>
 
            <xsl:text> ACRS AREA</xsl:text>
        </xsl:element>
        
        <!--  output all multiple level freezing hazard lines -->
        <xsl:for-each select="//Gfa[@hazard='M_FZLVL' and contains(@fcstHr,'-')] ">
            <xsl:variable name="flightLevelStatement">
                <xsl:call-template name="GetM_FZLVL_FlightLevels">
                    <xsl:with-param name="base" select="@bottom"/>
                    <xsl:with-param name="top" select="@top"/>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:value-of select="$newline"/>
            <xsl:element name="line">
                <xsl:attribute name="indent">3</xsl:attribute>
                <xsl:text>MULT FRZLVL </xsl:text><xsl:value-of select="$flightLevelStatement"/>
                <xsl:text> BOUNDED BY </xsl:text><xsl:value-of select="@textVor"/>
            </xsl:element>
            
        </xsl:for-each>
        
        <!--  output all freezing level contour lines -->
        
        <xsl:for-each select="//Gfa[@hazard='FZLVL' and contains(@fcstHr,'-') and (@level='SFC' or @level='000') and $ZuluArea=@area]">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="@level"/>
                <xsl:with-param name="fromLn" select="@textVor"/>
                <xsl:with-param name="closed" select="@closed"/>
            </xsl:call-template>
        </xsl:for-each>
        
        <xsl:for-each select="//Gfa[@hazard='FZLVL' and contains(@fcstHr,'-') and (@level='040')] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="@level"/>
                <xsl:with-param name="fromLn" select="@textVor"/>
                <xsl:with-param name="closed" select="@closed"/>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:for-each select="//Gfa[@hazard='FZLVL' and contains(@fcstHr,'-') and (@level='080')] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="@level"/>
                <xsl:with-param name="fromLn" select="@textVor"/>
                <xsl:with-param name="closed" select="@closed"/>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:for-each select="//Gfa[@hazard='FZLVL' and contains(@fcstHr,'-') and (@level='120')] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="@level"/>
                <xsl:with-param name="fromLn" select="@textVor"/>
                <xsl:with-param name="closed" select="@closed"/>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:for-each select="//Gfa[@hazard='FZLVL' and contains(@fcstHr,'-') and (@level='160')] ">
            <xsl:call-template name="OutputFzlvlContour">
                <xsl:with-param name="level" select="@level"/>
                <xsl:with-param name="fromLn" select="@textVor"/>
                <xsl:with-param name="closed" select="@closed"/>
            </xsl:call-template>
        </xsl:for-each>

        <!--  Add AMD/COR line -->
        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">FZLVL</xsl:with-param>
                <xsl:with-param name="haz2">M_FZLVL</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
         
        <xsl:if test="string-length($amdTest) > 1">    
            <xsl:variable name="AMD">
                <xsl:call-template name="SetStatus">
                    <xsl:with-param name="amdTest" select="$amdTest"/>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:choose>
                
                <xsl:when test="contains( $amdTest, 'AMD')">
                    <xsl:element name="line">
                        <xsl:value-of select="$newline"/>
                        <xsl:text>...UPDT...</xsl:text>
                    </xsl:element> 
                </xsl:when>
                
                <xsl:when test="contains( $amdTest, 'COR')">
                    <xsl:element name="line">
                        <xsl:value-of select="$newline"/>
                        <xsl:text>...CORRECTED FRZLVL...</xsl:text>
                    </xsl:element>
                </xsl:when>
                
            </xsl:choose>   
        </xsl:if>
        <!--   End AMD/COR line -->
        
    </xsl:template>

    <xsl:template name="GetZuluFreqSevStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
        <xsl:param name="base"/>
        <xsl:param name="top"/>
        <xsl:param name="fzlBase"/>
        <xsl:param name="fzlTop"/>
    	<xsl:param name="dueTo"/>
    	<xsl:param name="conditions"/>
        
        <xsl:variable name="flightLevelStatement">
            <xsl:call-template name="MakeFlightLevel">
                <xsl:with-param name="base" select="$base"/>
                <xsl:with-param name="top" select="$top"/>
            </xsl:call-template>
        </xsl:variable>


    	<xsl:if test="string-length($frequency) >1 or
    		          string-length($severity) > 1 or
    		          string-length($flightLevelStatement) > 1 or
    		          string-length($dueTo) > 1 or
    		          string-length($conditions) > 1" >
 
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
  	    
            <xsl:if test="string-length($severity)>1 and contains($hazard, 'ICE')">
                <xsl:text> MOD </xsl:text>
                <xsl:value-of select="$severity"/>
            </xsl:if>

            <xsl:if test="string-length($dueTo)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$dueTo"/>
            </xsl:if>

            <xsl:if test="string-length($flightLevelStatement)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$flightLevelStatement"/>
            </xsl:if>

    	 <xsl:if test="string-length($fzlBase) > 1" >
    	      <xsl:text>. FRZLVL </xsl:text>
    	      <xsl:value-of select="$fzlBase"/>
    	      <xsl:text>-</xsl:text>
    	      <xsl:value-of select="$fzlTop"/>
    	 </xsl:if>

    	 <xsl:if test="string-length($frequency) >1 or
    	        string-length($severity) > 1 or
    	        string-length($flightLevelStatement) > 1 or
    	        string-length($dueTo) > 1" >
    	     <xsl:text>. </xsl:text>       
    	 </xsl:if>
    	    
         <xsl:if test="string-length($conditions)>1" >
             <xsl:value-of select="$conditions"/>             
         </xsl:if>

        </xsl:if>

    </xsl:template>

    <xsl:template name="GetM_FZLVL_FlightLevels">
        <xsl:param name="base">Missing Base</xsl:param>
        <xsl:param name="top">Missing Top</xsl:param>
        
        <xsl:variable name="FLbase">
            <xsl:call-template name="GetFlightLevel">
                <xsl:with-param name="flightLevel" select="$base"/>
                <xsl:with-param name="useFL">0</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:choose>
            <xsl:when test="contains( $FLbase, 'SFC')">
                <xsl:text>BLW </xsl:text><xsl:value-of select="$top"/>
            </xsl:when>
            
            <xsl:otherwise>
                <xsl:value-of select="$base"/><xsl:text>-</xsl:text>
                <xsl:value-of select="$top"/>
            </xsl:otherwise>
            
        </xsl:choose>
        
    </xsl:template>

    <xsl:template name="OutputFzlvlContour">
        <xsl:param name="level">missing level</xsl:param>
        <xsl:param name="fromLn">missing From line</xsl:param>
        <xsl:param name="closed">missing close value</xsl:param>

        <xsl:value-of select="$newline"/>

        <xsl:element name="line">        
            
            <xsl:choose>
                <xsl:when test="contains($level, '000')">
                    <xsl:text>   SFC</xsl:text>
                </xsl:when>
                <xsl:otherwise>
        	   <xsl:text>   </xsl:text>
                    <xsl:value-of select="normalize-space($level)"/>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="contains($closed,'true')">
                    <xsl:text> BOUNDED BY </xsl:text>
                </xsl:when>
                <xsl:otherwise> ALG </xsl:otherwise>
            </xsl:choose>
           
            <xsl:value-of select="normalize-space($fromLn)"/>
        </xsl:element>
    </xsl:template>

   <xsl:template name="readFzlvlBase">
		<xsl:param name="FAarea">
			area
		</xsl:param>
		<xsl:choose>
			<xsl:when test="$numFzlvl > 0 ">
	<xsl:variable name="areaRange">
          <xsl:for-each select="//Gfa[@fzlRange]">
            <xsl:if test="contains(@fzlRange, $FAarea)">
	                        <xsl:value-of select="substring-after(substring-after(@fzlRange, $FAarea), ';')"/>
                	<xsl:text>;</xsl:text>
            </xsl:if>
	  </xsl:for-each>
	</xsl:variable>

				<xsl:value-of
					select="substring-before(substring-after($areaRange, ';'),';')" />

			</xsl:when>
			<xsl:otherwise>
				<xsl:for-each select="//Gfa[@hazard='M_FZLVL' and contains(@fcstHr,'-')] ">
					<xsl:value-of select="@bottom" />
				</xsl:for-each>

			</xsl:otherwise>
		</xsl:choose>
   </xsl:template>

   <xsl:template name="readFzlvlTop">
		<xsl:param name="FAarea">
			area
		</xsl:param>

		<xsl:choose>
			<xsl:when test="$numFzlvl > 0 ">
	<xsl:variable name="areaRange">
          <xsl:for-each select="//Gfa[@fzlRange]">
            <xsl:if test="contains(@fzlRange, $FAarea)">
	                        <xsl:value-of select="substring-after(substring-after(@fzlRange, $FAarea), ';')"/>
            </xsl:if>
	  </xsl:for-each>
	</xsl:variable>

	<xsl:value-of select="substring-before($areaRange, ';')"/>

			</xsl:when>
			<xsl:otherwise>
				<xsl:for-each select="//Gfa[@hazard='M_FZLVL' and contains(@fcstHr,'-')] ">

					<xsl:value-of select="@top" />
				</xsl:for-each>

			</xsl:otherwise>
		</xsl:choose>

    </xsl:template>

</xsl:stylesheet>
