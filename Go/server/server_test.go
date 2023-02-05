package main

import (
	"context"
	"fmt"
	"protein_synthesis/message"
	"testing"
)

const DNA_STRING = "TTTGGGAAATTTGGGTTTAAAUAG"
const MRNA_STRING = "UUUGGGAAAUUUGGGUUUAAAUAG"
const AMINOACID_STRING = "PheGlyLysPheGlyPheLysSTOP"
const VALID_DNA_STRING = "AUGTTTGGGTTTAAAAAAUAG"

func executeData(data message.Message) (string , bool){

	dnaDivisions := (len(data.Data)/3)
	inTranscription := make(chan DnaSlice, dnaDivisions)
	outTranscription := make(chan DnaSlice, dnaDivisions)
	translationChannel := make(chan DnaSlice, dnaDivisions)
	
	for i := 0; i < dnaDivisions; i++ {
		dividedDna, error := divideDNA(data.Data, i)

		if error != nil {
			fmt.Println("Error while dividing dna")
		}
		go handleTranscription(inTranscription, outTranscription, context.TODO())
		inTranscription <- DnaSlice{i, dividedDna}
		
		if data.Req == message.Translate{
			go handleTranslation(outTranscription, translationChannel, context.TODO())
		}
	}

	if data.Req == message.Transcribe{
		builderChannel := make(chan []DnaSlice)
		go buildResult(outTranscription, dnaDivisions, builderChannel, context.TODO())
		return buildResponse(<-builderChannel, data.Req)
		
	}

	builderChannel := make(chan []DnaSlice)
	go buildResult(translationChannel, dnaDivisions, builderChannel, context.TODO())
	return buildResponse(<-builderChannel, data.Req)

}

func buildResponse(resultData []DnaSlice, req message.ReqType) (string, bool){
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
	return msg, isValid
}

func TestTranscription(t *testing.T){

	result , _ := executeData(*message.NewTranscriptionRequest(DNA_STRING))

	if result != MRNA_STRING{
		t.Error("error in transcription")
	}

}

func TestTranslation(t *testing.T){

	result , isValid := executeData(*message.NewTranslationRequest(DNA_STRING))

	if result != AMINOACID_STRING{
		t.Error("error in translation")
	}

	if isValid {
		t.Error("error in validation")
	}

	_ , isValid = executeData(*message.NewTranslationRequest(VALID_DNA_STRING))

	if !isValid{
		t.Error("error in validation")
	}

}