package org.apache.qpid.client.message;
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


import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;

public class FiledTableSupport
{
  public static FieldTable convertToFieldTable(Map<String,?> props)
  {
      FieldTable ft = new FieldTable();
      if (props != null)
      {
          for (String key : props.keySet())
          {
              ft.setObject(key, props.get(key));
          }
      }
      return ft;
  }

  public static Map<String,Object> convertToMap(FieldTable ft)
  {
     Map<String,Object> map = new HashMap<String,Object>();
     for (AMQShortString key: ft.keySet() )
     {
         map.put(key.asString(), ft.getObject(key));
     }

     return map;
  }
}
