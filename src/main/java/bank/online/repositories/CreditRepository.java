package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.Credit;

@Repository
public interface CreditRepository extends JpaRepository<Credit, Long>{

	@Query(value ="SELECT * FROM credit cr INNER JOIN user usr "
			+ "ON cr.emprunteur_id = usr.id WHERE usr.id =:idUser "
			+ "AND cr.rembourse=0 ORDER BY cr.idcredit ASC LIMIT 1",nativeQuery=true)
	Credit getNoneRembourmentCreditByUser(@Param("idUser") Long idUser);
}
