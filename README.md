# COP5615 Bitcoin Mining Project

## Team Members

- Rishabh Srivastav : UFID 7659-9488
- Ashish Sunny Abraham : UFID 6388-7782

## Outline of the project

Using Erlang's Actor Model, we've created a simulation of bitcoin mining. To mine bitcoins with the required exact number of leading zeros, we employed the SHA256 algorithm. In addition, we've put in place IP address-based remote configuration between client and server devices.

## Commands to run the program 

- To run Server(server.erl) instance: '_erl -name server@{ipAddress} -setcookie {cookieName}_'

    - e.g.: '_erl -name server@10.20.170.60 -setcookie project_'
    - Compile server.erl using _c(server)_. This compiles the server side implementation of the code.
    - Compile client.erl using _c(client)_. This compiles the client side implementation of the code for distributed modeling.
    - Once you have entered the erl shell, to start the server: '_server:start()._'
      - Next '_Enter number of 0s to mine for bitcoin :_ '
      - Next '_Enter number of coins to mine :_ '
      - Next '_Enter number of workers to spawn :_ '
    - Server can mine coins without any active client nodes

- Distributed Implementation of the Project:
    - Start the server using the above steps, then create client nodes.
    - To create Client(client.erl) node: '_erl -name {clientName}@{ipAddress} -setcookie {cookieName}_'
      - e.g.: '_erl -name client@10.20.170.60 -setcookie project'_
      - To test the distributed implementation, enter 0 in Number of workers to spawn on the server side since we don't want          the server to mine coins by itself and want the client to mine.
      - Once you have entered the erl shell, to start the client: '_client:start("{ip\_address\_of\_server}")._'
      - e.g.: _client:start("10.20.170.60")._

## Implementation Details 

**File structure:**

- **Server.erl**

The server.erl is started with the number of leading zeros (Z_count), number of coins to mine (Max_Coins_Count) and the number of workers on each system (Miner_count). The server spawns workers (actors) to mine coins and then waits to receive messages either from the spawned actors that a coin is found, which the server will print, or from a client that wishes to participate in the mining. Any number of clients may connect. These actors on the client behave the same as actors on the server and will send a message to the server when a coin is found. The workers are designed to continue mining until the server finds the total number of coins given by the user.

- **Client.erl**

Client takes in the server IP address as the argument and a connection is established with the server after which actors on the client side begin mining for coins.

**Program Workflow:**

- The server will take input from the user regarding the number of zeroes to classify a coin as a bitcoin, number of coins that the user wants to mine and the number of worker nodes that the user wants to spawn.

- The server spawns workers/actors depending on the input given by the user using the spawn_many() function. The workers spawned on the server begin to mine bitcoins.

- If a worker process shuts down, the work assigned to it will be delegated by the server to some other remote worker active at that time.

- The server will start generating random strings that will append to "rishabhsrivastav;" and the workers will start mining the bitcoins using start_mining() function. If the worker finds a bitcoin, it sends a message to the server informing it found one and the server then prints the bitcoin.

- The distributed implementation can be executed by creating client nodes using steps mentioned above. Enter the number of workers to spawn as 0 as now we'll set up individual client nodes by following steps mentioned above. Once a node is created and you're inside the shell, type _**client:start("\<ipAddress\>").**_ and the client then informs the server that the connection has been established and starts to mine coins.

## Assignment Details
**Work Unit:**

Every actor is tasked with generating a random string, computing the hash, and mining for bitcoins. Each actor runs until the leading zero condition is satisfied and the generated hash is verified. Upon finding the number of coins given by the user, all the actors are killed. We determined that each actor should take on the complete responsibility of generating the string, hashing it, and checking if it's a valid coin. Thus, the actor now takes all the responsibility and the server only tells the actor whether to continue mining or not.

In our program we give the user the option to enter the number of workers to spawn with an upper threshold of _max 5 digit numerical(10,000)_. We specifically chose this work unit because

1. This will avoid the possibility of repeated generation of the same string across the workers and limit it to the first n number of miners required by the user.
2. This approach is scalable as different workers get different amounts of workload.

We have defined the ideal number of processes to run as = _No.of Cores ^ 4_. This ensures that all the cores are used efficiently to mine bitcoins in a faster manner.

**The result of running the program for input 4:**

We ran our program for _4 leading zeroes_, trying to find _50 bitcoins_ and spawned _10,000 actors_ receiving the following metrics:

Output received on Macbook pro (Specs: 10 core CPU, M1 pro chip):
![1](https://user-images.githubusercontent.com/70634373/192115790-e9525294-33f1-4936-bf3c-1123c19bb6a8.png)

Output received on Acer Nitro Windows 10 Laptop (Specs: 8 cores, i7-9th Gen Chip):
![combined](https://user-images.githubusercontent.com/70634373/192123077-108fcce1-c60e-4034-a996-936c4cfd0051.jpg)


**The ratio of CPU time to REAL time for the program:**

*MacBook Pro M1 Pro Chipset:*

Total clock time: 97475.792 ms

Total CPU time: 185577 ms

CPU Utilization: CPU time/ Run Time 1.9038265418761613

*Windows 10 Acer Nitro 8 core i7-9th gen Chipset:*

Total clock time: 14154.854 ms

Total CPU time: 101828 ms

CPU Utilization: CPU time/ Run Time 7.193857315660056

**The coin with the most 0s mined by our program:**.

We mined coins with _8 leading zeroes_ resulting in _2 coins_ within ~5 hours. We executed this on a Macbook Pro with M1 pro chip with configurations: 10-core CPU and 14-core GPU. 
![2](https://user-images.githubusercontent.com/70634373/192115729-fd99b350-3649-4341-88ec-6bb167226c83.png)

**The largest number of working machines we were able to run our code with:**

We were able to run our code on _3 different systems_ and they all were able to mine bitcoins simultaneously. This can also be scaled up to multiple different machines.

The client id of the 3 machines are:

1. **\<13761.88.0\>**
2. **\<13762.92.0\>**
3. **\<13763.88.0\>**

![Capture](https://user-images.githubusercontent.com/70634373/192118255-a4197dda-db5c-406a-af11-12c22482ccdb.PNG)
<img width="1061" alt="Screen Shot 2022-09-24 at 4 36 42 PM" src="https://user-images.githubusercontent.com/70634373/192118263-caa56635-32da-4a18-93f8-45a2ed3468a7.png">


