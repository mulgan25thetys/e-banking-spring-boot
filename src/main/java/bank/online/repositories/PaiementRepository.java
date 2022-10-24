package bank.online.repositories;

import java.util.Date;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.PaiementCredit;

@Repository
public interface PaiementRepository extends JpaRepository<PaiementCredit, Long>{

	@Query(value = "SELECT * FROM paiement_credit pc INNER JOIN credit c ON c.idcredit = pc.credit_idcredit "
			+ "INNER JOIN user u ON u.id = c.emprunteur_id WHERE u.id =:idEmprunteur AND "
			+ "(:today < pc.date_limit AND (pc.a_rembourse = 1 AND pc.date_remboursement IS null)) "
			+ "ORDER BY pc.date_limit ASC LIMIT 1",nativeQuery = true)
	Optional<PaiementCredit> getMonthlyMensualityToPay(@Param("today") Date today,@Param("idEmprunteur") Long idEmprunteur);
}
