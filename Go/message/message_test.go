package message

import (
	"testing"
)

const DNA_STRING = `tgttgcagctactgcagacttggttaacgcctgtgtcacccgtcctggtctgttcactatcggactgggagtaaagctgggtcgaatctcccatcgaagaacacgaagccttagttacagaggagacttgctttattgataggccgcatagccatgccgtctgatactcaataagattaaacgtttttaacaagcgctctcgtaaccctttagcctcccgcttttgtaggggccggcttggaaccacgagaatagttcctcccttgtcgaaatcttcacagactatatcccggcggtgcacgtccaagtgaaatgataagctcccagcactaaaccacggagacgcagaatgacaatgaagagaactcgcatgcttaagcctagatgttacttagcgcgttgcaaaaagactttatactggctctcttcacgaagggagccgacttgaagggcccagggagttttacgcttaccttatccttaccgaccttagcattacggattactgtgttatttgtgttcttctaaaccacaccaacttttctaaaactgcataaaccatttaggacccatctgtatccctcacctcccggaagacctggcccccgagcaccaaagggacagcaggttggactcttcgcatagagctccagcatgcgaatcttttctccttccggctaggatcacttagttggttgatcaggagtcttatcccgtgcgtatctgattcgttcaccgatgagcgggacgccagatcgctcgtggtacttctctgcactgcacttactataaattcgggttattgtgtggtttcggtgggctgtagagacgtcttcgtgggcgatgctgggggcgtgctattgctcttcatgcctctcggtcacctcctatgctttccccgcagacgtgccccgaccaccgatgtaggtgtataccggagctcggcttaaaaatggacatattgtgcaggccgaccggacgaccttttcgcgtaatacta`

func TestMarshallReqNoErr(t *testing.T) {
	msg1 := NewTranscriptionRequest(DNA_STRING)
	msg2 := NewTranslationRequest(DNA_STRING)
 	_, err := msg1.ToJSON()
	if err != nil {
		t.Error(err)
	}
	_, err = msg2.ToJSON()
	if err != nil {
		t.Error(err)
	}
}

func TestMarshallResultNoErr(t *testing.T) {
	msg1 := NewTranscriptionResult("UGUUGCAGCUACUGCAGACUUGGUUAACGCCUGUGUCACCCGUCCUGGUCUGUUCACUAUCGGACUGGGAGUAAAGCUGGGUCGAAUCUCCCAUCGAAGAACACGAAGCCUUAGUUACAGAGGAGACUUGCUUUAUUGAUAGGCCGCAUAGCCAUGCCGUCUGAUACUCAAUAAGAUUAAACGUUUUUAACAAGCGCUCUCGUAACCCUUUAGCCUCCCGCUUUUGUAGGGGCCGGCUUGGAACCACGAGAAUAGUUCCUCCCUUGUCGAAAUCUUCACAGACUAUAUCCCGGCGGUGCACGUCCAAGUGAAAUGAUAAGCUCCCAGCACUAAACCACGGAGACGCAGAAUGACAAUGAAGAGAACUCGCAUGCUUAAGCCUAGAUGUUACUUAGCGCGUUGCAAAAAGACUUUAUACUGGCUCUCUUCACGAAGGGAGCCGACUUGAAGGGCCCAGGGAGUUUUACGCUUACCUUAUCCUUACCGACCUUAGCAUUACGGAUUACUGUGUUAUUUGUGUUCUUCUAAACCACACCAACUUUUCUAAAACUGCAUAAACCAUUUAGGACCCAUCUGUAUCCCUCACCUCCCGGAAGACCUGGCCCCCGAGCACCAAAGGGACAGCAGGUUGGACUCUUCGCAUAGAGCUCCAGCAUGCGAAUCUUUUCUCCUUCCGGCUAGGAUCACUUAGUUGGUUGAUCAGGAGUCUUAUCCCGUGCGUAUCUGAUUCGUUCACCGAUGAGCGGGACGCCAGAUCGCUCGUGGUACUUCUCUGCACUGCACUUACUAUAAAUUCGGGUUAUUGUGUGGUUUCGGUGGGCUGUAGAGACGUCUUCGUGGGCGAUGCUGGGGGCGUGCUAUUGCUCUUCAUGCCUCUCGGUCACCUCCUAUGCUUUCCCCGCAGACGUGCCCCGACCACCGAUGUAGGUGUAUACCGGAGCUCGGCUUAAAAAUGGACAUAUUGUGCAGGCCGACCGGACGACCUUUUCGCGUAAUACUA")
	msg2 := NewTranslationResult("MetGlnGluAsnThrTyrValTrpHisLysLysSTOP",true)
	_, err := msg1.ToJSON()
	if err != nil {
		t.Error(err)
	}
	_, err = msg2.ToJSON()
	if err != nil {
		t.Error(err)
	}
}

func TestMarshallUnmarshallRequest(t *testing.T) {
	msg1 := NewTranscriptionRequest(DNA_STRING)
	msg2 := NewTranslationRequest(DNA_STRING)
	jmsg1, _ := msg1.ToJSON()
	jmsg2, _ := msg2.ToJSON()
	msg3, err1 := FromJSON(jmsg1)
	msg4, err2 := FromJSON(jmsg2)
	if err1 != nil {
		t.Error(err1)
	}
	if err2 != nil {
		t.Error(err2)
	}
	if *msg1 != *msg3 {
		t.Errorf("Request %v and %v are not the same.", msg1, msg3)
	}
	if *msg2 != *msg4 {
		t.Errorf("Request %v and %v are not the same.", msg2, msg4)
	}
}

func TestMarshallUnmarshallResult(t *testing.T) {
	msg1 := NewTranscriptionResult("UGUUGCAGCUACUGCAGACUUGGUUAACGCCUGUGUCACCCGUCCUGGUCUGUUCACUAUCGGACUGGGAGUAAAGCUGGGUCGAAUCUCCCAUCGAAGAACACGAAGCCUUAGUUACAGAGGAGACUUGCUUUAUUGAUAGGCCGCAUAGCCAUGCCGUCUGAUACUCAAUAAGAUUAAACGUUUUUAACAAGCGCUCUCGUAACCCUUUAGCCUCCCGCUUUUGUAGGGGCCGGCUUGGAACCACGAGAAUAGUUCCUCCCUUGUCGAAAUCUUCACAGACUAUAUCCCGGCGGUGCACGUCCAAGUGAAAUGAUAAGCUCCCAGCACUAAACCACGGAGACGCAGAAUGACAAUGAAGAGAACUCGCAUGCUUAAGCCUAGAUGUUACUUAGCGCGUUGCAAAAAGACUUUAUACUGGCUCUCUUCACGAAGGGAGCCGACUUGAAGGGCCCAGGGAGUUUUACGCUUACCUUAUCCUUACCGACCUUAGCAUUACGGAUUACUGUGUUAUUUGUGUUCUUCUAAACCACACCAACUUUUCUAAAACUGCAUAAACCAUUUAGGACCCAUCUGUAUCCCUCACCUCCCGGAAGACCUGGCCCCCGAGCACCAAAGGGACAGCAGGUUGGACUCUUCGCAUAGAGCUCCAGCAUGCGAAUCUUUUCUCCUUCCGGCUAGGAUCACUUAGUUGGUUGAUCAGGAGUCUUAUCCCGUGCGUAUCUGAUUCGUUCACCGAUGAGCGGGACGCCAGAUCGCUCGUGGUACUUCUCUGCACUGCACUUACUAUAAAUUCGGGUUAUUGUGUGGUUUCGGUGGGCUGUAGAGACGUCUUCGUGGGCGAUGCUGGGGGCGUGCUAUUGCUCUUCAUGCCUCUCGGUCACCUCCUAUGCUUUCCCCGCAGACGUGCCCCGACCACCGAUGUAGGUGUAUACCGGAGCUCGGCUUAAAAAUGGACAUAUUGUGCAGGCCGACCGGACGACCUUUUCGCGUAAUACUA")
	msg2 := NewTranslationResult("MetGlnGluAsnThrTyrValTrpHisLysLysSTOP",true)
	jmsg1, _ := msg1.ToJSON()
	jmsg2, _ := msg2.ToJSON()
	msg3, err1 := FromJSON(jmsg1)
	msg4, err2 := FromJSON(jmsg2)
	if err1 != nil {
		t.Error(err1)
	}
	if err2 != nil {
		t.Error(err2)
	}
	if *msg1 != *msg3 {
		t.Errorf("Message %v and %v are not the same.", msg1, msg3)
	}
	if *msg2 != *msg4 {
		t.Errorf("Message %v and %v are not the same.", msg2, msg4)
	}
}
