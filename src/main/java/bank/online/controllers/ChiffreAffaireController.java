package bank.online.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.repositories.PaiementRepository;

@RestController
@RequestMapping("stats")
public class ChiffreAffaireController {

	@Autowired
	PaiementRepository payCreditRepo;
	
	@GetMapping("get-chiffre-affaire-by-credit")
	@ResponseBody
	public ResponseEntity<Object> getChiffreAffaireByCredit() {
		
		return ResponseEntity.ok().body(payCreditRepo.getChiffreAffaireByCredit());
	}
	

}
