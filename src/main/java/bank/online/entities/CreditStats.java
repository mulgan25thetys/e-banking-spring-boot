package bank.online.entities;

import java.util.Date;

public class CreditStats {

	private CategorieCredit categorie;
	private Integer nombre;
	private Date dateAjout;
	
	public CreditStats() {
		super();
	}
	
	public CategorieCredit getCategorie() {
		return categorie;
	}
	public void setCategorie(CategorieCredit categorie) {
		this.categorie = categorie;
	}
	public Integer getNombre() {
		return nombre;
	}
	public void setNombre(Integer nombre) {
		this.nombre = nombre;
	}
	public Date getDateAjout() {
		return dateAjout;
	}
	public void setDateAjout(Date dateAjout) {
		this.dateAjout = dateAjout;
	}

	public CreditStats(CategorieCredit categorie, Integer nombre, Date dateAjout) {
		super();
		this.categorie = categorie;
		this.nombre = nombre;
		this.dateAjout = dateAjout;
	}
	
	
	
}
