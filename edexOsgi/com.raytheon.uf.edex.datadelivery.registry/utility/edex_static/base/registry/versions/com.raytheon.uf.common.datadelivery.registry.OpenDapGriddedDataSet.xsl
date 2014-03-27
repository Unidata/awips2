<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- 
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
 * OpenDapGriddedDataSet.xsl
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
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns2="com.raytheon.uf.common.datadelivery.registry"
    version="1.0">

    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Parameter.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Time.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Coverage.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Ensemble.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="openDapGriddedDataSet" name="openDapGriddedDataSet">

        <openDapGriddedDataSet
            xmlns:ns2="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns4="http://www.w3.org/1999/xlink" xmlns:ns3="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
            xmlns:ns9="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0"
            xmlns:ns5="http://www.w3.org/2005/08/addressing" xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns7="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0"
            xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0">

            <xsl:if test="@dataSetType != ''">
                <xsl:attribute name="dataSetType">
        		<xsl:value-of select="@dataSetType" />
    		</xsl:attribute>
            </xsl:if>
            <xsl:if test="@dataSetName != ''">
                <xsl:attribute name="dataSetName">
       			<xsl:value-of select="@dataSetName" />
    		</xsl:attribute>
            </xsl:if>
            <xsl:if test="@collectionName != ''">
                <xsl:attribute name="collectionName">
        		<xsl:value-of select="@collectionName" />
    		</xsl:attribute>
            </xsl:if>
            <xsl:if test="@providerName != ''">
                <xsl:attribute name="providerName">
        		<xsl:value-of select="@providerName" />
    		</xsl:attribute>
            </xsl:if>

            <!-- List of applied templates -->
            <xsl:apply-templates select="parameters" />
            <xsl:apply-templates select="coverage" />
            <xsl:apply-templates select="time" />
            <xsl:apply-templates select="ensemble" />

            <xsl:if test="availabilityOffset">
                <xsl:element name="availabilityOffset">
                    <xsl:value-of select="availabilityOffset" />
                </xsl:element>
            </xsl:if>

            <xsl:for-each select="cycles">
                <xsl:element name="cycles">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>
            <xsl:for-each select="forecastHours">
                <xsl:element name="forecastHours">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>

            <xsl:apply-templates select="cyclesToUrls" />

            <xsl:for-each select="cycleUpdate">
                <xsl:element name="cycleUpdate">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>

        </openDapGriddedDataSet>

    </xsl:template>

    <xsl:template match="parameters">
        <parameters>
            <xsl:for-each select="entry">
                <xsl:element name="entry">
                    <xsl:apply-templates select="key" />
                    <xsl:apply-templates select="value" />
                </xsl:element>
            </xsl:for-each>
        </parameters>
    </xsl:template>

    <xsl:template match="cyclesToUrls">
        <xsl:element name="cyclesToUrls">
            <xsl:for-each select="entry">
                <xsl:element name="entry">
                    <xsl:apply-templates select="key" />
                    <xsl:apply-templates select="value" />
                </xsl:element>
            </xsl:for-each>
        </xsl:element>
    </xsl:template>

    <xsl:template match="key">
        <xsl:choose>
            <xsl:when test="@xsi:type='xs:string'">
                <key xsi:type="xs:string" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <xsl:value-of select="." />
                </key>
            </xsl:when>
            <xsl:when test="@xsi:type='xs:int'">
                <key xsi:type="xs:int" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <xsl:value-of select="." />
                </key>
            </xsl:when>
            <xsl:otherwise>
                <!-- unknown type, do nothing -->
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="value">
        <xsl:choose>
            <xsl:when test="@xsi:type='xs:string'">
                <value xsi:type="xs:string" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <xsl:value-of select="." />
                </value>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="parameterValue" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>