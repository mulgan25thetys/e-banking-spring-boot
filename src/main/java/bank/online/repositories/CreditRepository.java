package bank.online.repositories;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.Credit;

@Repository
public interface CreditRepository extends JpaRepository<Credit, Long>{

	@Query(value ="SELECT * FROM credit cr INNER JOIN user usr "
			+ "ON cr.emprunteur_id = usr.id WHERE usr.id =:idUser "
			+ "AND (cr.rembourse=0 OR cr.status='ATTENTE') ORDER BY cr.idcredit ASC LIMIT 1",nativeQuery=true)
	Credit getNoneRembourmentCreditByUser(@Param("idUser") Long idUser);
	
	@Query(value = "SELECT * "
			+ "FROM credit WHERE date_ajout BETWEEN :date_deb "
			+ "AND :date_fin GROUP BY date_ajout ORDER BY date_ajout ASC",nativeQuery =true)
	List<Credit> getStatsDateAjout(@Param("date_deb") Date dateDeb,@Param("date_fin") Date dateFIn);
	
	@Query(value = "SELECT COUNT(date_ajout) FROM credit "
			+ "WHERE date_ajout BETWEEN :date_deb AND :date_fin "
			+ "GROUP BY date_ajout ORDER BY date_ajout ASC",nativeQuery =true)
	List<Integer> getStatsNombre(@Param("date_deb") Date dateDeb,@Param("date_fin") Date dateFIn);
}
