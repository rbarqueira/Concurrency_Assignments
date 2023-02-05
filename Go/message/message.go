// DO NOT MODIFY THIS FILE!

package message

import (
	"encoding/json"
	"fmt"
)

type MsgType int

type ReqType int

const (
	Request MsgType = iota
	Result
	Transcribe ReqType = iota
	Translate
)

// Message represents a message that can be sent between the
//server and a client of the protein synthesis project. The messages
//effectively are of two types: a client sends Requests to
//to the server, generally of the form:
//Message{Type:Request,ReqType:<type>,Data:<DNA>}
//hoping to receive back a message of the form:
//Message{Type:Result,ReqType:Transcribe,Data:<mRNA string>}
//or
//Message{Type:Result,ReqType:Translate,Data:<aminoacid chain>,Valid:<validity>}
//The first response applies to transcription requests, the second
//to translation requests, where the Data field is a string of aminoacid
//(abbreviated using the shorthand from the table in the assignment) and where
//the Valid field indicates whether the aminoacid chain forms a valid protein.
type Message struct {
	Type  MsgType
	Req   ReqType
	Data  string
	Valid bool
}

// ToJson() method serializes a Message into a JSON byte slice, ready
//to be sent over the wire.
func (msg *Message) ToJSON() ([]byte, error) {
	return json.Marshal(msg)
}

// FromJson() function takes a byte slice containing a JSON representation of
//a Message and unmarshals it back into a Go value.
func FromJSON(data []byte) (*Message, error) {
	var m Message
	err := json.Unmarshal(data, &m)
	return &m, err
}

// NewTranslationResult(data,valid) creates a result message for a translation
//request with the given data string and validity.
func NewTranslationResult(data string, valid bool) *Message {
	return &Message{
		Type:  Result,
		Req:   Translate,
		Data:  data,
		Valid: valid,
	}
}

// NewTranscriptionResult(data,valid) creates a result message for a
// transcription request with the given data string and validity.
func NewTranscriptionResult(data string) *Message {
	return &Message{
		Type: Result,
		Req:  Transcribe,
		Data: data,
	}
}

// NewTranscriptionRequest(data) creates a transcription request message for
//the given data.
func NewTranscriptionRequest(data string) *Message {
	return &Message{
		Type: Request,
		Req:  Transcribe,
		Data: data,
	}
}

// NewTranslationRequest(data) creates a translation request message
//for the given data.
func NewTranslationRequest(data string) *Message {
	return &Message{
		Type: Request,
		Req:  Translate,
		Data: data,
	}
}

func (msg *Message) String() string {
	if msg.Req == Translate {
		if msg.Type == Request {
			return fmt.Sprintf("Msg[Request Translation \"%s\"]", msg.Data)
		} else {
			return fmt.Sprintf("Msg[Result Translation \"%s\" %t]", msg.Data, msg.Valid)
		}

	} else {
		if msg.Type == Request {
			return fmt.Sprintf("Msg[Request Transcription \"%s\"]", msg.Data)
		} else {
			return fmt.Sprintf("Msg[Result Transcription \"%s\"]", msg.Data)
		}
	}
}
