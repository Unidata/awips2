<?xml version="1.0" encoding="ISO-8859-1"?><!-- 
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 * 
 * Parameter.xsl
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 ******************************************************************
 * Mar 02, 2014 2789      dhladky      XSLT transformation of other versions to current.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns9="com.raytheon.uf.common.datadelivery.registry"
    version="1.0">
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <!-- regular old parameter -->
    <xsl:template match="parameter" name="bareParameter">
        <xsl:element name="parameter">
            <xsl:call-template name="parameterAttributes" />
        </xsl:element>
    </xsl:template>

    <!-- Used when a parameter is namespace referenced. -->
    <xsl:template match="ns9:parameter" name="parameter">
        <ns9:parameter xmlns:ns2="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
            xmlns:ns4="http://www.w3.org/2005/08/addressing" xmlns:ns3="http://www.w3.org/1999/xlink"
            xmlns:ns9="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns5="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0"
            xmlns:ns7="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0"
            xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0">

            <xsl:call-template name="parameterAttributes" />
        </ns9:parameter>
    </xsl:template>


    <!-- Used when a parameter is nested in a map as a value, which it frequently 
        is. -->
    <xsl:template match="value" name="parameterValue">
        <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <xsl:attribute name="xsi:type">
    			<xsl:value-of select="@xsi:type" />
			</xsl:attribute>
            <xsl:call-template name="parameterAttributes" />
        </value>
    </xsl:template>

    <xsl:template match="levelType" name="levelType">
        <xsl:element name="levelType">
            <xsl:if test="@unit != ''">
                <xsl:attribute name="unit">
        			<xsl:value-of select="@unit" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@type != ''">
                <xsl:attribute name="type">
        			<xsl:value-of select="@type" />
    			</xsl:attribute>
            </xsl:if>

            <xsl:for-each select="layer">
                <xsl:element name="layer">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>
        </xsl:element>
    </xsl:template>

    <xsl:template match="levels">
        <xsl:element name="levels">
            <xsl:if test="@dz != ''">
                <xsl:attribute name="dz">
        			<xsl:value-of select="@dz" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@levelType != ''">
                <xsl:attribute name="levelType">
      		  		<xsl:value-of select="@levelType" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@name != ''">
                <xsl:attribute name="name">
       		 		<xsl:value-of select="@name" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@requestLevelEnd != ''">
                <xsl:attribute name="requestLevelEnd">
        			<xsl:value-of select="@requestLevelEnd" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@requestLevelStart != ''">
                <xsl:attribute name="requestLevelStart">
        			<xsl:value-of select="@requestLevelStart" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="level">
                <xsl:for-each select="level">
                    <xsl:element name="level">
                        <xsl:value-of select="." />
                    </xsl:element>
                </xsl:for-each>
            </xsl:if>
            <xsl:if test="selectedLevelIndices">
                <xsl:for-each select="selectedLevelIndices">
                    <xsl:element name="selectedLevelIndices">
                        <xsl:value-of select="." />
                    </xsl:element>
                </xsl:for-each>
            </xsl:if>
        </xsl:element>
    </xsl:template>

    <xsl:template name="parameterAttributes">
        <xsl:if test="@baseType != ''">
            <xsl:attribute name="baseType">
      		  	<xsl:value-of select="@baseType" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@fillValue != ''">
            <xsl:attribute name="fillValue">
   		     	<xsl:value-of select="@fillValue" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@missingValue != ''">
            <xsl:attribute name="missingValue">
       		 	<xsl:value-of select="@missingValue" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@dataType != ''">
            <xsl:attribute name="dataType">
  		      	<xsl:value-of select="@dataType" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@units != ''">
            <xsl:attribute name="units">
        		<xsl:value-of select="@units" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@definition != ''">
            <xsl:attribute name="definition">
        		<xsl:value-of select="@definition" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@providerName != ''">
            <xsl:attribute name="providerName">
        		<xsl:value-of select="@providerName" />
    		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@name != ''">
            <xsl:attribute name="name">
      		  	<xsl:value-of select="@name" />
    		</xsl:attribute>
        </xsl:if>

        <!-- List of applied templates -->
        <xsl:if test="levelType">
            <xsl:for-each select="levelType">
                <xsl:call-template name="levelType" />
            </xsl:for-each>
        </xsl:if>
        <xsl:if test="levels">
            <xsl:apply-templates select="levels" />
        </xsl:if>
    </xsl:template>

</xsl:stylesheet>
