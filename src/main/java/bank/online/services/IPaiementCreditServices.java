package bank.online.services;

import java.util.List;

import bank.online.entities.PaiementCredit;

public interface IPaiementCreditServices {

	List<PaiementCredit> addPaiements(List<PaiementCredit> paiements);
}
