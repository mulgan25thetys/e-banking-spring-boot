package bank.online.services;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import bank.online.entities.Devises;
import bank.online.repositories.DevisesRepository;

@Service
public class DevisesServicesImpl implements IDevisesServices{

	@Autowired
	DevisesRepository deviseRepo;
	
	@Override
	public List<Devises> findAll() {
		
		return deviseRepo.findAll();
	}

	@Override
	public Devises addDevise(Devises devise) {
		
		return deviseRepo.save(devise);
	}

	@Override
	public Devises editDevise(Devises devise) {
		devise.setDateModification(new Date());
		return deviseRepo.save(devise);
	}

	@Override
	public Devises getDevise(Long id) {
		return deviseRepo.findById(id).orElse(null);
	}

	@Override
	public void removeDevise(Long id) {
		Optional<Devises> gottenDevise = deviseRepo.findById(id);
		
		if(gottenDevise.isPresent()) {
			deviseRepo.delete(gottenDevise.get());
		}
	}
}
