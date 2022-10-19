package bank.online.services;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import bank.online.entities.ReseauPaiement;
import bank.online.repositories.ReseauPaiementRepository;

@Service
public class ReseauPaiementServicesImpl implements IReseauPaiementServices{

	@Autowired
	ReseauPaiementRepository rPRepo;

	@Override
	public List<ReseauPaiement> findAll() {
		
		return rPRepo.findAll();
	}

	@Override
	public ReseauPaiement addReseauPay(ReseauPaiement reseau) {
		reseau.setDateCreation(new Date());
		return rPRepo.save(reseau);
	}

	@Override
	public ReseauPaiement editReseauPay(ReseauPaiement reseau) {
		reseau.setDateModification(new Date());
		return rPRepo.save(reseau);
	}

	@Override
	public ReseauPaiement getReseauPaiement(Long id) {
		return rPRepo.findById(id).orElse(null);
	}

	@Override
	public void deleteReseauPay(Long id) {
		Optional<ReseauPaiement> gottenReseau = rPRepo.findById(id);
		if(gottenReseau.isPresent()) {
			rPRepo.delete(gottenReseau.get());
		}	
	}
}
