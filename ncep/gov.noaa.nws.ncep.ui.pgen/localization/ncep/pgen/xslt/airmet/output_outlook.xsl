<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:import href="get_attention_line.xsl"/>
    <xsl:import href="get_update.xsl"/>
    
    
    <!-- 
		OutputOutlookHdr
		
		This template outputs the header line (1st line) of the outlook portion of an airmet bulletin.
		
		Change Log
		====== ===
		E. Safford/SAIC    11/05        initial coding
		E. Safford/SAIC    02/06        fix hazard to reference param
		E. Safford/SAIC    03/06        correct state list reference
		E. Safford/SAIC    06/06        add updateFlag (AMD or COR) to first line of outlook
		B. Yin/SAIC	   03/08	add updateFlag only if there is one outlook
					if there are more than one, don't add in the header
    -->
    <xsl:template name="OutputOutlookHdr">
        <xsl:param name="outlookStart"/>
        <xsl:param name="outlookEnd"/>
        <xsl:param name="totalOutlooks">1</xsl:param>
        <xsl:param name="hazard"/>
        <xsl:param name="stateList"/>
        
        
        <xsl:variable name="updateFlag">
            <xsl:call-template name="GetUpdate">
                <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:element name="line">
            <xsl:value-of select="$newline"/><xsl:text>.</xsl:text></xsl:element>
        
        <xsl:element name="line">
            <xsl:value-of select="$newline"/>
            <xsl:text>OTLK VALID </xsl:text>
            <xsl:if test="string-length($outlookStart) > 1"><xsl:value-of select="substring($outlookStart, 3, 4)"/></xsl:if>
            <xsl:text>-</xsl:text>
            <xsl:if test="string-length($outlookEnd) > 1"><xsl:value-of select="substring($outlookEnd, 3, 4)"/></xsl:if>
            <xsl:text>Z</xsl:text>
            <xsl:if test="number($totalOutlooks) = number( 1 )">
                <xsl:text>...</xsl:text><xsl:value-of select="normalize-space($hazard)"/><xsl:text> </xsl:text> <xsl:value-of select="normalize-space($stateList)"/>
                <xsl:if test="number( string-length($updateFlag) ) > number( '1' )"> 
                    <xsl:value-of select="normalize-space($updateFlag)"/>
                </xsl:if>   
            </xsl:if>
        </xsl:element>
        
    </xsl:template>
    
    
    <!-- 
		OutputOutlook
		
		This template outputs an individual outlook paragraph.
		
		Change Log
		====== ===
		E. Safford/SAIC    11/05        initial coding
		E. Safford/SAIC    01/06        add stateList param, reposition Bounded By line
		E. Safford/SAIC    02/06        reposition elipse before hazard, param change for OutputOutlookHdr
		E. Safford/SAIC    02/06        generalize DUE_TO into freqSevStatement param
		B. Yin/SAIC	   07/07	add tag number
		B. Yin/SAIC	   03/08	add updateFlag in each outlook if there are more 
					than one outlooks
    -->
    <xsl:template name="OutputOutlook">
        <xsl:param name="outlookStart">missing outlook start</xsl:param>
        <xsl:param name="outlookEnd">missing outlook end</xsl:param>
        <xsl:param name="hazard">missing hazard</xsl:param>
        <xsl:param name="stateList">missing state list</xsl:param>
        <xsl:param name="outlookNumber">1</xsl:param>
        <xsl:param name="totalOutlooks">1</xsl:param>
        <xsl:param name="boundedBy">missing bounded by</xsl:param>
        <xsl:param name="expectedHazard"></xsl:param>
        <xsl:param name="freqSevStatement"></xsl:param>

        
        <!--  Put the header on the outlooks if this is the first/only outlook. -->
        <xsl:if test="number($outlookNumber) = number( 1 )">
            <xsl:call-template name="OutputOutlookHdr">
                    <xsl:with-param name="outlookStart" select="$outlookStart"/>
                    <xsl:with-param name="outlookEnd" select="$outlookEnd"/>
                    <xsl:with-param name="totalOutlooks" select="$totalOutlooks"/>
                    <xsl:with-param name="hazard" select="$hazard"/>
                    <xsl:with-param name="stateList" select="$stateList"/>
                </xsl:call-template>
        </xsl:if>

        <xsl:variable name="updateFlag">
            <xsl:call-template name="GetUpdate">
                <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="status"><xsl:value-of select="@issueType"/></xsl:variable>

        <!-- 
            For a multi-outlook report indicate the area number then hazard and state list.
            For single outlook reports this information is combined with the header line.  
            See OutputOutlookHdr above.
        -->
        <xsl:if test="$totalOutlooks > 1">
            <xsl:element name="line">
                 <xsl:value-of select="$newline"/>
                
                <xsl:text>AREA </xsl:text><xsl:value-of select="$outlookNumber"/><xsl:text>...</xsl:text>         
                <xsl:value-of select="$hazard"/><xsl:text> </xsl:text><xsl:value-of select="normalize-space($stateList)"/>

                <xsl:if test="number( string-length($updateFlag) ) > number( '1' )"> 
                    <xsl:value-of select="normalize-space($updateFlag)"/>
                </xsl:if>   

           </xsl:element>
        </xsl:if>
        
        <xsl:element name="line"> <xsl:value-of select="$newline"/>
            <xsl:text>BOUNDED BY </xsl:text><xsl:value-of select="normalize-space($boundedBy)"/>
        </xsl:element>
    
        <xsl:if test="string-length( $expectedHazard ) > 1">
            <xsl:element name="line">
                <xsl:value-of select="$newline"/>
                <xsl:value-of select="normalize-space($expectedHazard)"/><xsl:text>.</xsl:text>
            </xsl:element>    
        </xsl:if>
        
 <!--  -      <xsl:variable name="airTag"><xsl:value-of select="concat(@tag,@desk)"/></xsl:variable>--> 
        <xsl:variable name="airTag"><xsl:value-of select="@airmetTag"/></xsl:variable>

        <xsl:if test="not( contains( @issueType, 'CAN' )) and string-length( $freqSevStatement ) > 1">
            <xsl:element name="line">
                <xsl:value-of select="$newline"/>
                <xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if>
            </xsl:element> 
        </xsl:if>

        <xsl:if test="contains( @issueType, 'CAN' ) or not(string-length($freqSevStatement) > 1)">
        <xsl:if test="string-length($airTag) > 1">

            <xsl:element name="line">
                <xsl:value-of select="$newline"/><xsl:value-of select="normalize-space($airTag)"/>.</xsl:element>
        </xsl:if>
        </xsl:if>
        
        <!--  Add the attention line(s) -->
        <xsl:call-template name="GetAttentionLine">
            <xsl:with-param name="status"><xsl:value-of select="@issueType"/></xsl:with-param>
            <xsl:with-param name="airmet_outlook">OUTLOOK</xsl:with-param>
        </xsl:call-template>
        
        <xsl:if test="not($totalOutlooks = $outlookNumber)">
        	<xsl:value-of select="$newline"/>
            <xsl:element name="line"><xsl:attribute name="indent">0</xsl:attribute><xsl:text>.</xsl:text></xsl:element>                                  
        </xsl:if>
        
     </xsl:template>
</xsl:stylesheet>
