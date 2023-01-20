package bank.online.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.PaiementCredit;
import bank.online.repositories.CreditRepository;
import bank.online.repositories.PaiementRepository;
import bank.online.services.ICreditServices;
import bank.online.services.IPaiementCreditServices;

@CrossOrigin(origins = "*", maxAge = 3600)
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
		return paiementRepo.findAllGroupByMensuality();
	}
	
	@GetMapping("find-all-by-credit/{id}")
	@ResponseBody
	public List<PaiementCredit> findAllByCredit(@PathVariable("id") Long id){
		return paiementRepo.findAllByCredit(id);
	}
}

