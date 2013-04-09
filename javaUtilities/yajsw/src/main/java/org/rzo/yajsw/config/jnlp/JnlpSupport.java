package org.rzo.yajsw.config.jnlp;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.vfs2.FileObject;
import org.rzo.yajsw.util.VFSUtils;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class JnlpSupport
{
	Document	_doc	= null;

	public JnlpSupport(String file) throws ParserConfigurationException, SAXException, IOException
	{
		FileObject fo = VFSUtils.resolveFile(".", file);
		if (fo == null || !fo.exists())
			throw new FileNotFoundException(file);
		InputStream in = fo.getContent().getInputStream();
		_doc = parseJnlp(in);
		in.close();
	}

	public PropertiesConfiguration toConfiguration(String defaultsFile) throws ConfigurationException, IOException
	{
		int i = 1;
		PropertiesConfiguration jnlpConf = new PropertiesConfiguration();

		List jars = getJars(_doc);
		for (Iterator it = jars.listIterator(); it.hasNext();)
		{
			jnlpConf.setProperty("wrapper.java.classpath." + i++, it.next());
		}

		jnlpConf.setProperty("wrapper.base", getCodebase(_doc));

		jnlpConf.setProperty("wrapper.java.app.mainclass", getMainClass(_doc));

		i = 1;
		for (Iterator it = getArguments(_doc).listIterator(); it.hasNext();)
		{
			jnlpConf.setProperty("wrapper.app.parameter." + i++, it.next());
		}
		i = 1;
		List props = getResourceProperties(_doc);
		for (Iterator it = props.listIterator(); it.hasNext();)
		{
			jnlpConf.setProperty("wrapper.java.additional." + i++, it.next());
		}

		i = 1;
		List resources = getResources(_doc);
		for (Iterator it = resources.listIterator(); it.hasNext();)
		{
			jnlpConf.setProperty("wrapper.resource." + i++, it.next());
		}

		if (defaultsFile == null || "".equals(defaultsFile))
			return jnlpConf;

		// jnlpConf.addProperty("include", defaultsFile);

		if (defaultsFile != null)
		{
			PropertiesConfiguration defaultsConf = new PropertiesConfiguration();
			FileObject fo = VFSUtils.resolveFile(".", defaultsFile);
			InputStream in = fo.getContent().getInputStream();
			defaultsConf.load(in);
			in.close();
			for (Iterator it = defaultsConf.getKeys(); it.hasNext();)
			{
				String key = (String) it.next();
				if (jnlpConf.containsKey(key))
					System.out.println("configuration conflict: " + key);
				else
					jnlpConf.addProperty(key, defaultsConf.getProperty(key));
			}
		}

		return jnlpConf;

	}

	private Document parseJnlp(InputStream in) throws ParserConfigurationException, SAXException, IOException
	{
		Document doc = null;
		// Get Document Builder Factory
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		// Turn off validation, and turn off namespaces
		factory.setValidating(false);
		factory.setNamespaceAware(false);

		DocumentBuilder builder = factory.newDocumentBuilder();

		doc = builder.parse(in);

		return doc;
	}

	private static List getJars(Document doc)
	{
		ArrayList result = new ArrayList();
		NodeList args = doc.getElementsByTagName("jar");
		for (int i = 0; i < args.getLength(); i++)
		{
			result.add(args.item(i).getAttributes().getNamedItem("href").getTextContent());
		}
		return result;
	}

	private static List getResources(Document doc)
	{
		ArrayList result = new ArrayList();
		NodeList args = doc.getElementsByTagName("icon");
		for (int i = 0; i < args.getLength(); i++)
		{
			result.add(args.item(i).getAttributes().getNamedItem("href").getTextContent());
		}
		return result;
	}

	private static List getResourceProperties(Document doc)
	{
		ArrayList result = new ArrayList();
		NodeList args = doc.getElementsByTagName("property");
		for (int i = 0; i < args.getLength(); i++)
		{
			String key = args.item(i).getAttributes().getNamedItem("name").getTextContent();
			String value = args.item(i).getAttributes().getNamedItem("value").getTextContent();
			result.add("-D" + key + "=" + value);
		}
		return result;
	}

	private static List getArguments(Document doc)
	{
		ArrayList result = new ArrayList();
		NodeList args = doc.getElementsByTagName("argument");
		for (int i = 0; i < args.getLength(); i++)
		{
			result.add(args.item(i).getTextContent());
		}
		return result;
	}

	private static Object getMainClass(Document doc)
	{
		return doc.getElementsByTagName("application-desc").item(0).getAttributes().getNamedItem("main-class").getTextContent();
	}

	private static String getCodebase(Document doc)
	{
		return doc.getElementsByTagName("jnlp").item(0).getAttributes().getNamedItem("codebase").getTextContent();
	}

}
