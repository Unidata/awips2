<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:lcm="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0"
    xmlns:query="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0" xmlns:spi="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0"
    xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:rs="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
    xmlns:ns5="http://www.w3.org/2005/08/addressing" xmlns:rim="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

    <xsl:template match="/">
        <html>
            <head>
                <style type="text/css">
                    a {font-weight:bold;}
                    td {width:300px;}
                    button{width:190px;}
                    table {border:0;}
                    body { font-family: Helvetica;
                    margin-left: 75px;
                    margin-right: 75px;
                    background: #D3D3D3;}
                </style>

                <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
                    <title>Registry Query Results</title>
                </meta>
            </head>
            <body>
                <h2>Query Result</h2>
                <table border="1">

                    <xsl:for-each select="query:QueryResponse/rim:ObjectRefList">
                        <tr bgcolor="#9acd32">
                            <th colspan="2">Registry Object Refs</th>
                        </tr>
                        <xsl:for-each select="rim:ObjectRef">
                            <tr>
                                <td>
                                    <xsl:value-of select="@id" />
                                </td>
                            </tr>
                        </xsl:for-each>

                    </xsl:for-each>

                    <xsl:for-each select="query:QueryRequest">
                        <tr bgcolor="#9acd32">
                            <th colspan="2">Query Request</th>
                        </tr>
                        <tr>
                            <th>Attribute</th>
                            <th>Value</th>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>Query Language</b>
                            </td>
                            <td>
                                <xsl:value-of
                                    select="query:Query/rim:Slot[@name='queryLanguage']/rim:SlotValue/rim:Value" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>Query Expression</b>
                            </td>
                            <td>
                                <xsl:value-of
                                    select="query:Query/rim:Slot[@name='queryExpression']/rim:SlotValue/rim:Value" />
                            </td>
                        </tr>
                    </xsl:for-each>

                    <xsl:for-each
                        select="query:QueryResponse/rim:RegistryObjectList/rim:RegistryObject">

                        <tr bgcolor="#9acd32">
                            <th colspan="3">Registry Object</th>
                        </tr>
                        <tr>
                            <th>Attribute</th>
                            <th>Value</th>
                            <th>Type</th>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>id</b>
                            </td>
                            <td>
                                <xsl:value-of select="@id" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>lid</b>
                            </td>
                            <td>
                                <xsl:value-of select="@lid" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>Object Type</b>
                            </td>
                            <td>
                                <xsl:value-of select="@objectType" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right">
                                <b>Owner</b>
                            </td>
                            <td>
                                <xsl:value-of select="@owner" />
                            </td>
                        </tr>
                        <xsl:for-each select="rim:Slot">
                            <tr>
                                <td align="right">
                                    <b>
                                        <xsl:value-of select="@name" />
                                    </b>
                                </td>
                                <td>
                                    <xsl:value-of select="rim:SlotValue/rim:Value" />
                                </td>
                                <td>
                                    <xsl:value-of select="rim:SlotValue/@xsi:type" />
                                </td>
                            </tr>
                        </xsl:for-each>
                    </xsl:for-each>
                </table>
            </body>
        </html>
    </xsl:template>

</xsl:stylesheet> 