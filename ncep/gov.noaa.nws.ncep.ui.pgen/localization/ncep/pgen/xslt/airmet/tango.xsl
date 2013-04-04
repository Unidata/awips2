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
	
	<xsl:template name="tango">

		<xsl:param name="area"/>

		<xsl:variable name="numSmearTango" select="count(//Gfa[@hazard='TURB' and contains(@fcstHr,'-') and contains(@area, $area)])"/>
		<xsl:variable name="numSmearSFC_WND" select="count(//Gfa[@hazard='SFC_WND' and contains(@fcstHr,'-') and contains(@area, $area)])"/>
		<xsl:variable name="numSmearLLWS" select="count(//Gfa[@hazard='LLWS' and contains(@fcstHr,'-') and contains(@area, $area)])"/>
		<xsl:variable name="numSmears" select="$numSmearTango +$numSmearSFC_WND + $numSmearLLWS"/>
		
		<xsl:variable name="numOutlookTURB" select="count(//Gfa[@hazard='TURB' and @isOutlook='true'])"/>
		<xsl:variable name="numOutlookSFC_WND" select="count(//Gfa[@hazard='SFC_WND' and @isOutlook='true'])"/>
		<xsl:variable name="numOutlookLLWS" select="count(//Gfa[@hazard='LLWS' and @isOutlook='true'])"/>
		<xsl:variable name="numOutlooks" select="$numOutlookTURB + $numOutlookSFC_WND + $numOutlookLLWS"/>

		<xsl:variable name="hazardTest">
			<xsl:call-template name="GetHazardsT"/>
		</xsl:variable>

		<xsl:variable name="untilTime">
			<xsl:call-template name="GetUntilTimeT"/>
		</xsl:variable>

		<xsl:variable name="outlookEndTime">
			<xsl:call-template name="GetOutlookEndTimeT"/>
		</xsl:variable>
		
		<xsl:variable name="issueTime">
			<xsl:call-template name="GetIssueTimeT"/>
		</xsl:variable>

		<xsl:variable name="hazards">
			<xsl:call-template name="SetHazards">
				<xsl:with-param name="hazardList" select="$hazardTest"/>
			</xsl:call-template>
		</xsl:variable>

		<xsl:variable name="amdTest">
			<xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">TURB</xsl:with-param>
                <xsl:with-param name="haz2">SFC_WND</xsl:with-param>
                <xsl:with-param name="haz3">LLWS</xsl:with-param>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:variable name="AMD">
			<xsl:call-template name="SetStatus">
				<xsl:with-param name="amdTest" select="$amdTest"/>
			</xsl:call-template>
		</xsl:variable>
                    
		<xsl:call-template name="OutputHeader">
			<xsl:with-param name="report">TANGO</xsl:with-param>
			<xsl:with-param name="hazard" select="$hazards"/>
			<xsl:with-param name="untilTime" select="$untilTime"/>
			<xsl:with-param name="faArea" select="$area" />
			<xsl:with-param name="issueTime" select="$issueTime" />
			<xsl:with-param name="amend" select="$AMD"/>
		</xsl:call-template>

	    <!--  If no TURB hazards, output no TURB report -->
		<xsl:if test="not($numSmearTango > 0)">
			<xsl:value-of select="$newline"/>
			<xsl:call-template name="NoHazardReport">
				<xsl:with-param name="expectedHazard">TURB</xsl:with-param>
			</xsl:call-template>
		</xsl:if>
		
		<!--  If any hazards, output all -->
		<xsl:if test="$numSmears > 0">
			<xsl:call-template name="TangoAirmet">
				<xsl:with-param name="faArea" select="$area"/>
				<xsl:with-param name="issueTime" select="$issueTime"/>
				<xsl:with-param name="untilTime" select="$untilTime"/>
		    </xsl:call-template>
		</xsl:if>

		<!--  output any outlooks -->
		<xsl:if test="$numOutlooks > 0">
	        <xsl:call-template name="TangoOutlook">
	            <xsl:with-param name="outlookStart" select="$untilTime"/>
	            <xsl:with-param name="outlookEnd" select="$outlookEndTime"/>
	            <xsl:with-param name="numTURB" select="$numOutlookTURB"/>
	            <xsl:with-param name="numSFC_WND" select="$numOutlookSFC_WND"/>
	            <xsl:with-param name="numLLWS" select="$numOutlookLLWS"/>
	            <xsl:with-param name="numOutlooks" select="$numOutlooks"/>
	        </xsl:call-template>
		</xsl:if>
		
		<!-- add bulletin footer -->
		<xsl:call-template name="OutputFooter"/>
			
	</xsl:template>
	
    <xsl:template name="GetHazardsT">
        <xsl:for-each select="//Gfa">
            <xsl:value-of select="@hazard"/>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="SetHazards">
        <xsl:param name="hazardList"/>
        <xsl:choose>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'SFC_WND') and contains( $hazardList, 'LLWS')">TURB STG WNDS AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'SFC_WND')">TURB AND STG SFC WNDS</xsl:when>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'LLWS')">TURB AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'SFC_WND') and contains($hazardList, 'LLWS')">TURB STG WNDS AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'SFC_WND')">TURB AND STG SFC WNDS</xsl:when>
            <xsl:when test="contains($hazardList, 'LLWS')">TURB AND LLWS</xsl:when>
            <xsl:otherwise>TURB</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

	<xsl:template name="GetUntilTimeT">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='TURB' or @hazard='SFC_WND' or @hazard='LLWS') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@untilTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>
	
	<xsl:template name="GetOutlookEndTimeT">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='TURB' or @hazard='SFC_WND' or @hazard='LLWS') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@outlookEndTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>
	
	<xsl:template name="GetIssueTimeT">
		<xsl:variable name="temp">
			<xsl:for-each select="//Gfa[(@hazard='TURB' or @hazard='SFC_WND' or @hazard='LLWS') and contains(@fcstHr,'-')][last()]">
				<xsl:value-of select="@issueTime"/>
			</xsl:for-each>
		</xsl:variable>
		<xsl:value-of select="$temp"/>
	</xsl:template>

    <xsl:template name="TangoAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">TURB</xsl:with-param>
                <xsl:with-param name="haz2">LLWS</xsl:with-param>
                <xsl:with-param name="haz3">SFC_WND</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="hazardTest">
                <xsl:call-template name="GetHazardsT"/>
        </xsl:variable>
        
        <xsl:variable name="hazards">
            <xsl:call-template name="SetHazards">
                <xsl:with-param name="hazardList" select="$hazardTest"/>
            </xsl:call-template>
        </xsl:variable>

        <!-- output Tango Airmets -->
		<xsl:call-template name="OutputTango">
		    <xsl:with-param name="hazType">TURB</xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="OutputTango">
		    <xsl:with-param name="hazType">SFC_WND</xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="OutputTango">
		    <xsl:with-param name="hazType">LLWS</xsl:with-param>
		</xsl:call-template>

    </xsl:template>

    <xsl:template name="TangoOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numTURB">0</xsl:param>
        <xsl:param name="numSFC_WND">0</xsl:param>
        <xsl:param name="numLLWS">0</xsl:param>
        <xsl:param name="numOutlooks">0</xsl:param>
        
        <!--   output all TURB outlooks  -->
        <xsl:for-each select="//Gfa[@hazard='TURB' and @isOutlook='true']">    
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard">TURB</xsl:with-param>
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
                <xsl:call-template name="GetTangoFreqSevStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
                    <xsl:with-param name="severity"><xsl:value-of select="@type"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="$hazardName"/>
                    <xsl:with-param name="top" select="@top"/>
                    <xsl:with-param name="base" select="@bottom"/>
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
                
        <!--   output all SFC WND outlooks  -->
        <xsl:for-each select="//Gfa[@hazard='SFC_WND' and @isOutlook='true']">
      
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard">SFC_WND</xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="expHazard">
                <xsl:call-template name="GetExpHazard">
                    <xsl:with-param name="haz" select="@hazard"/>
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
                <xsl:call-template name="GetTangoFreqSevStatement">                
                    <xsl:with-param name="expectedHaz" select="$expHazard"></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
          
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart" select="$outlookStart"/>
                <xsl:with-param name="outlookEnd" select="$outlookEnd"/>     
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="@states"/>
                <xsl:with-param name="outlookNumber" select="position() + $numTURB"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="@textVor"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>
            </xsl:call-template>
            
        </xsl:for-each>
        
    </xsl:template>

    <xsl:template name="OutputTango"> 
        <xsl:param name="hazType"/>

        <xsl:for-each select="//Gfa[(@hazard='TURB' or @hazard='SFC_WND' or @hazard='LLWS') and contains(@fcstHr,'-') and @isOutlook='false']">
            

            <xsl:if test="@hazard = $hazType">
                
                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                        <xsl:with-param name="hazard"><xsl:value-of select="$hazType"/></xsl:with-param>
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
                
                <xsl:variable name="expHazard">
                    <xsl:call-template name="GetExpHazard">
                        <xsl:with-param name="haz" select="@hazard"/>
                    </xsl:call-template>
                </xsl:variable>

                <xsl:variable name="freqSevStatement">
                    <xsl:call-template name="GetTangoFreqSevStatement">
           	        <xsl:with-param name="frequency"><xsl:value-of select="@frequency"/></xsl:with-param>
           	        <xsl:with-param name="severity"><xsl:value-of select="@type"/></xsl:with-param>
           	        <xsl:with-param name="hazard" select="$hazardName"/>
                              <xsl:with-param name="top" select="@top"/>
                              <xsl:with-param name="base" select="@bottom"/>
           	        <xsl:with-param name="dueTo"><xsl:value-of select="@dueTo"/></xsl:with-param>
                              <xsl:with-param name="expectedHaz" select="$expHazard"></xsl:with-param>
           	        <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
      	            </xsl:call-template>
                </xsl:variable>
 
                <xsl:variable name="status"><xsl:value-of select="@issueType"/></xsl:variable>
      
                <xsl:variable name="airmetStatement">
                    <xsl:call-template name="GetAirmetStatement">
                        <xsl:with-param name="haz"><xsl:value-of select="$hazardName"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="fromLineStatement">
                    <xsl:call-template name="GetFromLineStatement">
                        <xsl:with-param name="haz" select="@hazard"/>
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
             	    
                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">   
                    <xsl:value-of select="$newline"/><xsl:value-of select="$airmetStatement"/>...<xsl:value-of select="normalize-space(@states)"/>
                    <xsl:if test="string-length($updateFlag) > 1">
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                    </xsl:if>
                </xsl:element>

                <!-- From line -->
                <xsl:element name="line">
                    <xsl:value-of select="$newline"/><xsl:value-of select="$fromLineStatement"/><xsl:text> </xsl:text><xsl:value-of select="normalize-space(@textVor)"/></xsl:element>
                
                <!-- Frequency & Severity line -->
                <!--  <xsl:variable name="airTag"><xsl:value-of select="concat(@tag,@desk)"/></xsl:variable> -->
                <xsl:variable name="airTag"><xsl:value-of select="@airmetTag"/></xsl:variable>

                <xsl:if test="not( contains(@issueType, 'CAN' )) and string-length($freqSevStatement) > 1">
                    <xsl:element name="line">
                        <xsl:value-of select="$newline"/><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                 </xsl:if>

                    <!--  Add the attention line(s) -->
                    <xsl:call-template name="GetAttentionLine">
                        <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
                    </xsl:call-template>
                
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

	<xsl:template name="GetExpHazard">
	    <xsl:param name="haz">{hazard}</xsl:param>
	        <xsl:choose>
	            <xsl:when test="contains($haz, 'SFC_WND')">SUSTAINED SURFACE WINDS GTR THAN 30KT EXP</xsl:when>
	        </xsl:choose>
	        <xsl:choose>
	            <xsl:when test="contains($haz, 'LLWS')">LLWS EXP</xsl:when>
	        </xsl:choose>
	    
	</xsl:template>

    <xsl:template name="GetAirmetStatement">
        <xsl:param name="haz">{hazard}</xsl:param>
            <xsl:choose>
                <xsl:when test="contains($haz, 'LLWS')">LLWS POTENTIAL</xsl:when>
                <xsl:otherwise>AIRMET <xsl:value-of select="$haz"/></xsl:otherwise>
            </xsl:choose>        
    </xsl:template>

    <xsl:template name="GetFromLineStatement">
        <xsl:param name="haz">{hazard}</xsl:param>
            <xsl:choose>
                <xsl:when test="contains($haz, 'LLWS')">BOUNDED BY</xsl:when>
                <xsl:otherwise>FROM</xsl:otherwise>
            </xsl:choose>
    </xsl:template>

    <xsl:template name="GetTangoFreqSevStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
        <xsl:param name="base"/>
        <xsl:param name="top"/>
    	<xsl:param name="dueTo"/>
        <xsl:param name="expectedHaz"/>
    	<xsl:param name="conditions"/>

		<xsl:if test="string-length($frequency) >1 or
    		          string-length($severity) > 1 or
    		          string-length($top) > 1 or
    		          string-length($dueTo) > 1 or
	                  string-length($expectedHaz) > 1 or
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
	 	    
		 <xsl:variable name="flightLevelStatement">
		     <xsl:if test="contains( $hazard, 'TURB')">
		         <xsl:call-template name="MakeFlightLevel">
		             <xsl:with-param name="base" select="$base"/>
		             <xsl:with-param name="top" select="$top"/>
		         </xsl:call-template>
		     </xsl:if>
		 </xsl:variable>   
	    
            <xsl:if test="string-length($severity)>1 and contains($hazard, 'TURB')">
                <xsl:text> </xsl:text>
                <xsl:value-of select="$severity"/>
            </xsl:if>

            <xsl:if test="string-length($hazard) > 1 and contains($hazard, 'TURB')">
                <xsl:text> </xsl:text>
                   <xsl:choose>
                		<xsl:when test="$hazard = 'TURB'">MOD TURB</xsl:when>
	                	<xsl:otherwise><xsl:value-of select="$hazard"/></xsl:otherwise>
            	</xsl:choose>        
            </xsl:if>

            <xsl:if test="string-length($flightLevelStatement)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$flightLevelStatement"/>
            </xsl:if>

            <xsl:if test="string-length($dueTo)>1" >
                <xsl:text> </xsl:text>
                <text>DUE TO </text><xsl:value-of select="$dueTo"/>
            </xsl:if>
	    
            <xsl:if test="string-length($frequency) >1 or
		        string-length($severity) > 1 or
		        string-length($top) > 1 or
		        string-length($dueTo) > 1" >
	     <xsl:text>. </xsl:text>       
            </xsl:if>
	    
	 <xsl:if test="string-length($expectedHaz) > 1">
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$expectedHaz"/>
	     <xsl:text>. </xsl:text>
	 </xsl:if>	    	    	 
	    	        
            <xsl:if test="string-length($conditions)>1" >
                <xsl:value-of select="$conditions"/>             
            </xsl:if>	            

        </xsl:if>

    </xsl:template>

</xsl:stylesheet>
