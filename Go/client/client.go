package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"protein_synthesis/message"
)

func main() {
	transcribeFlag := flag.Bool("transcribe",false,"Transcription request")
	translateFlag := flag.Bool("translate",false,"Translation request")
	flag.Parse()
	if (!*transcribeFlag && !*translateFlag) {
		fmt.Println("Error: Specify a type of request.")
		
	} else {
		host := flag.Args()[0]
		scanner := bufio.NewScanner(os.Stdin)	
		scanner.Scan()
		dna := scanner.Text()
		var msg *message.Message
		if *transcribeFlag {
			msg = message.NewTranscriptionRequest(dna)
		} else {
			msg = message.NewTranslationRequest(dna)
		}
		// Your code goes here.

		connectionChannel := make(chan bool)
		connectTotheServer(host, connectionChannel, msg)

		<-connectionChannel
	}
}

func connectTotheServer(host string, connectionChannel chan bool, msg * message.Message){
	conn, err := net.Dial("tcp", host)
	if err != nil {
		printDisconnected()
		log.Fatal(err)
	}
	
	fmt.Println("Host is " + host)

	json, erorJson := msg.ToJSON()

	if erorJson != nil{
		fmt.Println("Error in JSON conversion")
		conn.Close()
	}

	json = append(json, '\n')

	conn.Write(json)
	
	readResponseFromServer(conn)
	defer conn.Close()
}

func readResponseFromServer(connection net.Conn){
	
	fmt.Println("Waiting for response")
	bufReader := bufio.NewReader(connection)
	dataBytes, errorData := bufReader.ReadBytes('\n')

	if errorData != nil {
		if errorData != io.EOF {
            log.Printf("Read error: %s", errorData)
        }
		connection.Close()
		fmt.Println("Error reading data from server")
	}

	data , errorJson := message.FromJSON(dataBytes)

	if errorJson != nil {
		connection.Close()
		fmt.Println("Error converting data from server")
	}

	fmt.Println("Response from server: ", data)
	
}

// printDisconnected prints a disconnected message to stdout.
func printDisconnected() {
	fmt.Println("Disconnected from the server.")
}
