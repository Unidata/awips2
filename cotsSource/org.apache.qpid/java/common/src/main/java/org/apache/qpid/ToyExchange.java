package org.apache.qpid;
/*
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * 
 */


import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.qpid.transport.MessageTransfer;


public class ToyExchange
{
  final static String DIRECT = "amq.direct";
  final static String TOPIC = "amq.topic";
  
  private Map<String,List<LinkedBlockingQueue<MessageTransfer>>> directEx = new HashMap<String,List<LinkedBlockingQueue<MessageTransfer>>>();
  private Map<String,List<LinkedBlockingQueue<MessageTransfer>>> topicEx = new HashMap<String,List<LinkedBlockingQueue<MessageTransfer>>>(); 
  private Map<String,LinkedBlockingQueue<MessageTransfer>> queues = new HashMap<String,LinkedBlockingQueue<MessageTransfer>>(); 
  
  public void createQueue(String name)
  {
      queues.put(name, new LinkedBlockingQueue<MessageTransfer>());
  }
  
  public LinkedBlockingQueue<MessageTransfer> getQueue(String name)
  {
      return queues.get(name);
  }
  
  public void bindQueue(String type,String binding,String queueName)
  {
      LinkedBlockingQueue<MessageTransfer> queue = queues.get(queueName);
      binding = normalizeKey(binding);
      if(DIRECT.equals(type))
      {
          
          if (directEx.containsKey(binding))
          {
              List<LinkedBlockingQueue<MessageTransfer>> list = directEx.get(binding);
              list.add(queue);
          }
          else
          {
              List<LinkedBlockingQueue<MessageTransfer>> list = new LinkedList<LinkedBlockingQueue<MessageTransfer>>();
              list.add(queue);
              directEx.put(binding,list);
          }
      }
      else
      {
          if (topicEx.containsKey(binding))
          {
              List<LinkedBlockingQueue<MessageTransfer>> list = topicEx.get(binding);
              list.add(queue);
          }
          else
          {
              List<LinkedBlockingQueue<MessageTransfer>> list = new LinkedList<LinkedBlockingQueue<MessageTransfer>>();
              list.add(queue);
              topicEx.put(binding,list);
          }
      }
  }
  
  public boolean route(String dest, String routingKey, MessageTransfer msg)
  {
      List<LinkedBlockingQueue<MessageTransfer>> queues;
      if(DIRECT.equals(dest))
      {
          queues = directEx.get(routingKey);          
      }
      else
      {
          queues = matchWildCard(routingKey);
      }
      if(queues != null && queues.size()>0)
      {
          System.out.println("Message stored in " + queues.size() + " queues");
          storeMessage(msg,queues);
          return true;
      }
      else
      {
          System.out.println("Message unroutable " + msg);
          return false;
      }
  }
  
  private String normalizeKey(String routingKey)
  {
      if(routingKey.indexOf(".*")>1)
      {
          return routingKey.substring(0,routingKey.indexOf(".*"));
      }
      else
      {
          return routingKey;
      }
  }
  
  private List<LinkedBlockingQueue<MessageTransfer>> matchWildCard(String routingKey)
  {        
      List<LinkedBlockingQueue<MessageTransfer>> selected = new ArrayList<LinkedBlockingQueue<MessageTransfer>>();
      
      for(String key: topicEx.keySet())
      {
          Pattern p = Pattern.compile(key);
          Matcher m = p.matcher(routingKey);
          if (m.find())
          {
              for(LinkedBlockingQueue<MessageTransfer> queue : topicEx.get(key))
              {
                    selected.add(queue);
              }
          }
      }
      
      return selected;      
  }

  private void storeMessage(MessageTransfer msg,List<LinkedBlockingQueue<MessageTransfer>> selected)
  {
      for(LinkedBlockingQueue<MessageTransfer> queue : selected)
      {
          queue.offer(msg);
      }
  }
  
}
