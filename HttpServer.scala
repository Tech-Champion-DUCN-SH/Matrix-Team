import java.net.{InetAddress, Socket, ServerSocket}
import java.util.concurrent.{Executors, ExecutorService}
import io.Source
import java.io.{File, IOException, InputStream, OutputStream}


class HttpServer(port: Int, pool_size: Int, server_name: String) extends Runnable
{
    val server_socket = new ServerSocket(port, 1, InetAddress.getByName(server_name.substring(0,server_name.indexOf(":"))))
    val pool: ExecutorService = Executors.newFixedThreadPool(pool_size)
    val node_list = new NodeList(2*pool_size)
    val node_id_manager = new NodeIDManager(node_list, 2*pool_size)

    var transaction_number = 0

    def getTransactionNumber(): Long =
    {
        return transaction_number
    }

    def run()
    {
       try
       {
           node_id_manager.init()
           node_list.init()
           (new Thread(new StatisticsCounter(this, node_list))).start()
           while(true)
           {
               val client_socket = server_socket.accept()
               transaction_number += 1
               pool.execute(new RequestHandler(client_socket, node_id_manager, node_list, this, server_name))
           }
       }
       finally
       {
           pool.shutdown()
       }
    }
}

object OURLS 
{
    val ROOT = System.getProperty("user.dir") + File.separator + "root"
    val SHUTDOWN = "/SHUTDOWN"
    var server_name = ""
    var thread_number = 1

    def main(args: Array[String])
    {
       // init server name and thread number
       var parameter = Array[String]()
       var fileName = "loadTimeParameter.ini"
       val br = new java.io.BufferedReader(new java.io.FileReader(fileName))
       thread_number = (br.readLine()).toInt
       server_name = br.readLine()
       br.close()
       val server = new HttpServer(8080, thread_number, server_name)
       server.run()
    }
}
