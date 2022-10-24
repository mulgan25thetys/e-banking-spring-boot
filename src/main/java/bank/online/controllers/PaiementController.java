package bank.online.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.PaiementCredit;
import bank.online.repositories.CreditRepository;
import bank.online.repositories.PaiementRepository;
import bank.online.services.ICreditServices;
import bank.online.services.IPaiementCreditServices;

@RestController
@RequestMapping("credit-paiements")
public class PaiementController {

	@Autowired
	CreditRepository creditRepo;
	
	@Autowired
	ICreditServices creditService;
	
	@Autowired
	IPaiementCreditServices payCreditService;
	
	@Autowired
	PaiementRepository paiementRepo;
	
	@GetMapping("find-all")
	@ResponseBody
	public List<PaiementCredit> findAll(){
		return paiementRepo.findAll();
	}
}

