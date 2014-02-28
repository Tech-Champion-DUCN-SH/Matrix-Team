import io.Source
//import java.io.{IOException, InputStream}
import java.net.{InetAddress, Socket, ServerSocket}
import java.io._



class RequestHandler(client_socket: Socket, node_id_manager: NodeIDManager, var node_list : NodeList, var server : HttpServer, var server_name : String) extends Runnable
{
    var uri : String = _

    def parse(request: String): String = 
    {
        val pattern = """[^  ]* *([^ ]*) *[\s\S]*""".r
        val pattern(result) = request
        result
    }

    def run()
    {
      try 
      { 
        val input = client_socket.getInputStream()
        val output = client_socket.getOutputStream()

        val lines = Source.fromInputStream(input).getLines()
        if(lines.hasNext)
        {
            uri = parse(lines.next())
        }

        val long_pattern = "^/urlshortener/url[?]{1}(longUrl)=(http[s]{0,1}[:]{1}//.+)".r
        val short_pattern = "/urlshortener/url[?]{1}(shortUrl)=http[:]{1}//.+/([a-zA-Z0-9]{1,6})".r
        var operation_type = ""
        var origin_url = ""
        var short_url = ""

        if(uri.length() <= 19)
        {
            // send erro response
        }
        else if(uri.charAt(18) == 'l')
        {
            try { 
              // ...
            val long_pattern(service_type, url) = uri
            operation_type = service_type
            origin_url = url
            } catch {
             case e: Exception =>
            }

        }
        else if(uri.charAt(18) == 's')
        {
            try { 
              // ...
                          val short_pattern(service_type, url) = uri

            operation_type = service_type
            short_url = url
            } catch {
               case e: Exception => 
            }

            
        }
        else if(uri == "/urlshortener/Statistics")
        {
            operation_type = "Statistics"
        }

        if( operation_type == "longUrl")//Align with url format
        {
            var node_id = node_id_manager.getNodeID()
            var node = node_list.getNodeById(node_id)
            var node_id_index = node.insert(origin_url)
            var id = node_id * node.size() + node_id_index

            var short_url = Base62.encode(id)
            
            output.write("HTTP/1.1 200 OK\r\nContent-Type:application/json;charset:utf-8\r\n\r\n".getBytes())
            output.write(("""{"kind": "shorten","shortUrl": "http://""" + server_name + "/"+ short_url+"""","longUrl": """+"\""+origin_url+"\"}").getBytes())
        }
        else if(operation_type == "shortUrl")//Align with url format
        {

            var id = Base62.decode(short_url)
            var node_id = id / 20000
            var node_id_index = id % 20000
            var long_url = node_list.getData(node_id.toInt, node_id_index.toInt)
            if(long_url!=null)
            {
            output.write("HTTP/1.1 200 OK\r\nContent-Type:application/json;charset:utf-8\r\n\r\n".getBytes())
            output.write(("""{"kind": "expand","shortUrl": "http://""" + server_name + "/" + short_url+"""","longUrl": """+"\""+long_url+"\"}").getBytes())
            }
            else
            {
            output.write("HTTP/1.1 200 OK\r\n".getBytes())
            output.write("Content-Type:application/json;charset:utf-8\r\n".getBytes())
            output.write("\r\n".getBytes())
            output.write(("""{"error": true,"code": 404,"message": "Not found"}""").getBytes())
            }
        }
        else if(operation_type == "Statistics")
        {
            val obj = new StatisticsCounter(server, node_list)

            val transaction_number = server.getTransactionNumber()
            val records_number = node_list.travelNodeList()
            val cpu_usage = obj.getCpuUsage()
            val mem_usage = obj.getMemUsage()
            output.write("HTTP/1.1 200 OK\r\n".getBytes())
            output.write("Content-Type:application/json;charset:utf-8\r\n".getBytes())
            output.write("\r\n".getBytes())
            output.write(("""{"kind": "Statistics","transaction_number": """+transaction_number+""","records_number": """+records_number+""","cpu_usage" : """+cpu_usage+""","mem_usage": """+mem_usage+"}").getBytes())

            // send result to client
        }
        else  
        {
            output.write("HTTP/1.1 200 OK\r\n".getBytes())
            output.write("Content-Type:application/json;charset:utf-8\r\n".getBytes())
            output.write("\r\n".getBytes())
            output.write(("""{"error": true,"code": 404,"message": "Not found"}""").getBytes())
        }

        client_socket.close()
        } 
    catch 
    {
        case e: Exception =>
    }
    }
}
