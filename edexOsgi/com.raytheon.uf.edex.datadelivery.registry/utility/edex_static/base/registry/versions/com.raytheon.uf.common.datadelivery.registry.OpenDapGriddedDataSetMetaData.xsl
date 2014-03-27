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
 * OpenDapGriddedDataSetMetaData.xsl
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
    version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Time.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="openDapGriddedDataSetMetaData"
        name="openDapGriddedDataSetMetaData">

        <openDapGriddedDataSetMetaData
            xmlns:ns2="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns4="http://www.w3.org/1999/xlink" xmlns:ns3="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
            xmlns:ns9="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0"
            xmlns:ns5="http://www.w3.org/2005/08/addressing" xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns7="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0"
            xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0">

            <xsl:if test="@cycle != ''">
                <xsl:attribute name="cycle">
        		<xsl:value-of select="@cycle" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@dataSetDescription != ''">
                <xsl:attribute name="dataSetDescription">
        		<xsl:value-of select="@dataSetDescription" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@dataSetName != ''">
                <xsl:attribute name="dataSetName">
        		<xsl:value-of select="@dataSetName" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@date != ''">
                <xsl:attribute name="date">
        		<xsl:value-of select="@date" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@providerName != ''">
                <xsl:attribute name="providerName">
        		<xsl:value-of select="@providerName" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@url != ''">
                <xsl:attribute name="url">
        		<xsl:value-of select="@url" />
			</xsl:attribute>
            </xsl:if>

            <!-- List of applied templates -->
            <xsl:apply-templates select="time" />

            <xsl:if test="availabilityOffset">
                <xsl:element name="availabilityOffset">
                    <xsl:value-of select="availabilityOffset" />
                </xsl:element>
            </xsl:if>

            <xsl:apply-templates select="levelTypes" />

        </openDapGriddedDataSetMetaData>
    </xsl:template>

    <xsl:template match="levelTypes">
        <xsl:element name="levelTypes">
            <xsl:for-each select="entry">
                <xsl:element name="entry">
                    <xsl:apply-templates select="key" />
                    <xsl:apply-templates select="value" />
                </xsl:element>
            </xsl:for-each>
        </xsl:element>
    </xsl:template>

    <xsl:template match="key">
        <key xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <xsl:if test="@xsi:type != ''">
                <xsl:attribute name="xsi:type">
        		<xsl:value-of select="@xsi:type" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@type != ''">
                <xsl:attribute name="type">
        		<xsl:value-of select="@type" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@unit != ''">
                <xsl:attribute name="unit">
        		<xsl:value-of select="@unit" />
			</xsl:attribute>
            </xsl:if>
            <xsl:for-each select="layer">
                <xsl:element name="layer">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>
        </key>
    </xsl:template>

    <xsl:template match="value">
        <value xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <xsl:if test="@xsi:type != ''">
                <xsl:attribute name="xsi:type">
        		<xsl:value-of select="@xsi:type" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@dz != ''">
                <xsl:attribute name="dz">
        		<xsl:value-of select="@dz" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@type != ''">
                <xsl:attribute name="type">
        		<xsl:value-of select="@type" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@name != ''">
                <xsl:attribute name="name">
        		<xsl:value-of select="@name" />
			</xsl:attribute>
            </xsl:if>
            <xsl:for-each select="level">
                <xsl:element name="level">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:for-each>
        </value>
    </xsl:template>

</xsl:stylesheet>
