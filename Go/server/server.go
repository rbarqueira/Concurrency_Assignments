package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"net"
	"os"
	"protein_synthesis/message"
	"strconv"
	"strings"
)

type DnaSlice struct {
	index int
	slice string
}

var aminoacids = map[string]string{
    "UUU": "Phe", "UUC": "Phe", "UUA": "Leu", "UUG": "Leu",
    "UCU": "Ser", "UCC": "Ser", "UCA": "Ser", "UCG": "Ser",
    "UAU": "Tyr", "UAC": "Tyr", "UAA": "STOP", "UAG": "STOP",
    "UGU": "Cys", "UGC": "Cys", "UGA": "STOP", "UGG": "Trp",

    "CUU": "Leu", "CUC": "Leu", "CUA": "Leu", "CUG": "Leu",
    "CCU": "Pro", "CCC": "Pro", "CCA": "Pro", "CCG": "Pro",
    "CAU": "His", "CAC": "His", "CAA": "Gin", "CAG": "Gin",
    "CGU": "Arg", "CGC": "Arg", "CGA": "Arg", "CGG": "Arg",

    "AUU": "Ile", "AUC": "Ile", "AUA": "Ile", "AUG": "Met",
    "ACU": "Thr", "ACC": "Thr", "ACA": "Thr", "ACG": "Thr",
    "AAU": "Asn", "AAC": "Asn", "AAA": "Lys", "AAG": "Lys",
    "AGU": "Ser", "AGC": "Ser", "AGA": "Arg", "AGG": "Arg",

    "GUU": "Val", "GUC": "Val", "GUA": "Val", "GUG": "Val",
    "GCU": "Ala", "GCC": "Ala", "GCA": "Ala", "GCG": "Ala",
    "GAU": "Asp", "GAC": "Asp", "GAA": "Glu", "GAG": "Glu",
    "GGU": "Gly", "GGC": "Gly", "GGA": "Gly", "GGG": "Gly",
}

func startServer(port int) (net.Listener, error) {

	listener, err := net.Listen("tcp", "localhost:"+strconv.Itoa(port))

	return listener, err
}

func runServer(listener net.Listener) {
	for {
		fmt.Println("Waiting for connection")
		conn, errorConnection := listener.Accept()
		if errorConnection != nil {
			log.Print(errorConnection)
			continue
		}
		go handleConnection(conn)
	}
}

func handleConnection(connection net.Conn) {

	bufReader := bufio.NewReader(connection)
	dataBytes, errorData := bufReader.ReadBytes('\n')

	if errorData != nil {
		connection.Close()
		fmt.Println("Error while reading data from connection")
	}

	data, errorJson := message.FromJSON(dataBytes)

	if errorJson != nil {
		connection.Close()
		fmt.Println("Error while converting message from JSON, ", errorJson)
	}

	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)

	go checkConnection(connection, cancel)

	executeMessage(*data, connection, ctx)

}

func checkConnection(connection net.Conn, cancel context.CancelFunc){
	for{
		one := make([]byte, 1)
		_, err := connection.Read(one)
		if err != nil{
			fmt.Println("Connection lost")
			cancel()
			return
		}
	}
}

func executeMessage(data message.Message, connection net.Conn, ctx context.Context){
	dnaDivisions := (len(data.Data)/3)
	inTranscription := make(chan DnaSlice, dnaDivisions)
	outTranscription := make(chan DnaSlice, dnaDivisions)
	translationChannel := make(chan DnaSlice, dnaDivisions)
	
	for i := 0; i < dnaDivisions; i++ {
		dividedDna, error := divideDNA(data.Data, i)

		if error != nil {
			fmt.Println("Error while dividing dna")
			connection.Close()
		}
		go handleTranscription(inTranscription, outTranscription, ctx)
		inTranscription <- DnaSlice{i, dividedDna}
		
		if data.Req == message.Translate{
			go handleTranslation(outTranscription, translationChannel, ctx)
		}
	}

	if data.Req == message.Transcribe{
		builderChannel := make(chan []DnaSlice)
		go buildResult(outTranscription, dnaDivisions, builderChannel, ctx)
		sendResponse(<-builderChannel, connection, data.Req)
		return
	}

	builderChannel := make(chan []DnaSlice)
	go buildResult(translationChannel, dnaDivisions, builderChannel, ctx)
	sendResponse(<-builderChannel, connection, data.Req)
				
}

func handleTranslation(outTranscription <-chan DnaSlice, translationChannel chan<- DnaSlice, ctx context.Context){
	select{
	case dnaSlice := <-outTranscription:
		dnaSlice.slice = aminoacids[dnaSlice.slice]
		translationChannel<-dnaSlice
	case <-ctx.Done():
		return
	}
}

func divideDNA(dna string, index int) (string, error) {

	start := 3*index
	end := start+3
	bytes := dna[start:end]

	return strings.ToUpper(string(bytes)), nil
}

func handleTranscription(in <-chan DnaSlice, out chan<- DnaSlice,  ctx context.Context) {
	select{
	case request := <-in:
		response := transcription(request)
		out<-response
	case <- ctx.Done():
		return
	}
}

func transcription(dnaSlice DnaSlice) DnaSlice {
	i := 0
	b := []byte(dnaSlice.slice)
	for _, charVariable := range dnaSlice.slice {
		if charVariable == 'T' || charVariable == 't' {
			b[i] = 'U'
		}
		i++
	}
	dnaSlice.slice = string(b)
	return dnaSlice
}

func buildResult(resultChannel <-chan DnaSlice, dataLength int, builderChannel chan []DnaSlice, ctx context.Context){
	var result []DnaSlice = initializeResultSlice(dataLength)

	for{
		select{
		case dnaSlice := <-resultChannel:
			result[dnaSlice.index] = dnaSlice
			if isBuildComplete(result){
				builderChannel <- result
				return
			}
			case <-ctx.Done():
				return 
		}
	}
}

func initializeResultSlice(dataLength int) []DnaSlice{
	var result []DnaSlice

	for i := 0 ; i < dataLength ; i++{
		result = append(result, DnaSlice{})
	}
	
	return result
}

func isBuildComplete(result []DnaSlice) bool{
	for i:= 0 ; i < len(result) ; i++{
		if result[i].slice == ""{
			return false
		}
	}
	return true
}

func sendResponse(resultData []DnaSlice, connection net.Conn, req message.ReqType){
	var msg string
	isValid := true

	for i := 0 ; i < len(resultData) ; i++{
		if req == message.Translate {
			if (i == 0 || i == len(resultData)-1) && isValid {
				isValid = validateTranslation(resultData[i])
			}
		}
		msg += resultData[i].slice
	}

	var messageJson []byte
	var errorJson error

	if req == message.Transcribe{
		messageJson , errorJson = message.NewTranscriptionResult(msg).ToJSON()
	}else{
		messageJson , errorJson = message.NewTranslationResult(msg, isValid).ToJSON()
	}


	if errorJson != nil{
		fmt.Println("Error converting response mesage to JSON")
		return;
	}

	messageJson = append(messageJson, '\n')

	connection.Write(messageJson)
}

func validateTranslation(dnaSlice DnaSlice) bool{

	if dnaSlice.index == 0{
		return dnaSlice.slice == "Met"
	}

	if dnaSlice.slice != "STOP"{
		return false
	}

	return true

}

var LOGF *log.Logger

func main() {
	// You may need a logger for debugging
	const (
		name = "log.txt"
		flag = os.O_RDWR | os.O_CREATE
		perm = os.FileMode(0666)
	)

	file, err := os.OpenFile(name, flag, perm)
	if err != nil {
		return
	}
	defer file.Close()

	LOGF = log.New(file, "", log.Lshortfile|log.Lmicroseconds)
	// Usage: LOGF.Println() or LOGF.Printf()

	const numArgs = 2
	if len(os.Args) != numArgs {
		fmt.Printf("Usage: ./%s <port>", os.Args[0])
		return
	}

	port, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("Port must be a number:", err)
		return
	}

	listener, serverError := startServer(port)
	if serverError != nil {
		log.Fatal(err)
	}

	fmt.Println("Server listening on port", port)

	runServer(listener)

	// TODO: implement this!
}
